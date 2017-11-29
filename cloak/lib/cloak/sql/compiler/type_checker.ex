defmodule Cloak.Sql.Compiler.TypeChecker do
  @moduledoc """
  Provides functions to check whether the expressions used in a query are valid according to our anonymization rules.
  This includes checks to determine if the number of restricted functions used (like functions that truncate values
  and could be used to create boolean logic circumventing other checks) exceed allowed thresholds, as well
  as checks to validate that columns used in certain filter conditions haven't been altered.
  """

  alias Cloak.Sql.{CompilationError, Condition, Expression, Query, Range}
  alias Cloak.Sql.Compiler.TypeChecker.{Narrative, Type}
  alias Cloak.Sql.Compiler.Helpers

  @max_allowed_restricted_functions 5


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec validate_allowed_usage_of_math_and_functions(Query.t) :: Query.t
  def validate_allowed_usage_of_math_and_functions(query) do
    each_subquery(query, &verify_usage_of_potentially_crashing_functions/1)
    each_subquery(query, &verify_allowed_usage_of_math/1)
    each_subquery(query, &verify_lhs_of_in_is_clear/1)
    each_subquery(query, &verify_not_equals_is_clear/1)
    each_subquery(query, &verify_lhs_of_not_like_is_clear/1)
    each_subquery(query, &verify_string_based_conditions_are_clear/1)
    each_subquery(query, &verify_ranges_are_clear/1)
    query
  end


  # -------------------------------------------------------------------
  # Top-level checks
  # -------------------------------------------------------------------

  defp verify_usage_of_potentially_crashing_functions(%Query{columns: columns} = query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Query.Lenses.operands()
    |> Lens.to_list(query)
    |> Enum.concat(columns)
    |> Enum.each(fn(column) ->
      type = Type.establish_type(column, query)
      if potentially_crashing_function?(type) do
        explanation = type.history_of_restricted_transformations
        |> filter_transformations([:potentially_crashing_function])
        |> Narrative.construct(type.history_of_columns_involved)
        raise CompilationError, message: """
          #{explanation}

          Functions are not allowed to be used in ways that could cause a database exception.
          This situation arises when a column or constant value is divided (`/`)
          by an expression that both contains a database column as well as a constant value
          (for example `age / (age - 20)`), or if the square root is taken of an expression that
          contains a database column as well as a constant value (for example `sqrt(age - 20)`).
          Please note that the system will also classify certain uses of math as potentially being
          a constant, such as `div(age, age)`.
          """
      end
    end)

  def verify_allowed_usage_of_math(query), do:
    Query.Lenses.analyst_provided_expressions()
    |> Lens.to_list(query)
    |> List.flatten()
    |> Enum.each(fn(expression) ->
      type = Type.establish_type(expression, query)
      if restricted_transformations_count(type) > @max_allowed_restricted_functions do
        explanation = type.history_of_restricted_transformations
        |> filter_transformations([:restricted_function])
        |> Narrative.construct(type.history_of_columns_involved)
        raise CompilationError, message: """
          #{explanation}

          Queries containing expressions with a high number of functions that are used in combination
          with constant values are prohibited. For further information about when this condition is
          triggered, please check the restrictions section of the user guides.
          """
      end
    end)

  @allowed_in_functions ~w(lower upper substring trim ltrim rtrim btrim)
  defp verify_lhs_of_in_is_clear(query), do:
    verify_conditions(query, &Condition.in?/1, fn({:in, lhs, _}) ->
      unless clear_lhs?(lhs, query, @allowed_in_functions) do
        raise CompilationError, message:
          "Only #{function_list(@allowed_in_functions)} can be used in the left-hand side of an IN operator."
      end
    end)

  @allowed_not_equals_functions ~w(lower upper substring trim ltrim rtrim btrim)
  defp verify_not_equals_is_clear(query), do:
    verify_conditions(query, &Condition.not_equals?/1, fn({:comparison, lhs, :<>, rhs}) ->
      if Type.establish_type(rhs, query).constant? do
        unless clear_lhs?(lhs, query, @allowed_not_equals_functions), do:
          raise CompilationError, message:
            "Only #{function_list(@allowed_not_equals_functions)} can be used in the arguments of an <> operator."
      else
        unless Type.establish_type(lhs, query).raw_column? and Type.establish_type(rhs, query).raw_column?, do:
          raise CompilationError, message: "When comparing two database columns with <> they cannot be modified."
      end
    end)

  defp verify_string_based_conditions_are_clear(query), do:
    verify_conditions(query, &(Condition.equals?(&1) or Condition.not_equals?(&1)), fn({:comparison, lhs, _, rhs}) ->
      lhs_type = Type.establish_type(lhs, query)
      rhs_type = Type.establish_type(rhs, query)

      if lhs_type.unclear_string_manipulation?, do:
        raise CompilationError, message: "String manipulation functions cannot be combined with other transformations."

      if lhs_type.string_manipulation? and not rhs_type.cast_raw_column?, do:
        raise CompilationError, message: "Results of string manipulation functions can only be compared to constants."
    end)

  @allowed_like_functions []
  defp verify_lhs_of_not_like_is_clear(query), do:
    verify_conditions(query, &Condition.not_like?/1, fn({:not, {kind, lhs, _}}) ->
      unless clear_lhs?(lhs, query, @allowed_like_functions) do
        raise CompilationError, message:
          "NOT #{like_kind_name(kind)} can only be applied to an unmodified database column."
      end
    end)

  defp function_list(function_names), do: function_names |> Enum.map(&"`#{&1}`") |> Enum.join(", ")

  defp like_kind_name(:like), do: "LIKE"
  defp like_kind_name(:ilike), do: "ILIKE"

  defp clear_lhs?(%Expression{aggregate?: true, function_args: [lhs]}, query, allowed_functions), do:
    clear_lhs?(lhs, query, allowed_functions)
  defp clear_lhs?(%Expression{function?: true, function: function, function_args: args}, query, allowed_functions), do:
    (function in allowed_functions) and clear_lhs?(main_argument(args), query, allowed_functions)
  defp clear_lhs?(lhs, query, _allowed_functions), do: Type.establish_type(lhs, query).raw_column?

  defp main_argument(args), do: Enum.at(args, 0)

  defp verify_ranges_are_clear(query), do:
    query
    |> Range.find_ranges()
    |> Enum.each(fn(%Range{column: column, interval: interval}) ->
      unless clear_range_lhs?(column, query, interval) do
        raise CompilationError, message: "Only unmodified database columns can be limited by a range."
      end
    end)

  defp clear_range_lhs?(%Expression{aggregate?: true, function_args: [lhs]}, query, interval), do:
    clear_range_lhs?(lhs, query, interval)
  defp clear_range_lhs?(lhs, query, :implicit), do: Type.establish_type(lhs, query).raw_implicit_range?
  defp clear_range_lhs?(lhs, query, _), do: Type.establish_type(lhs, query).cast_raw_column?

  defp verify_conditions(query, predicate, action), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(predicate)
    |> Lens.to_list(query)
    |> Enum.each(action)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp each_subquery(query, function), do:
    Helpers.apply_bottom_up(query, fn(subquery) ->
      function.(subquery)
      subquery
    end)

  defp potentially_crashing_function?(type), do:
    Enum.any?(type.history_of_restricted_transformations, fn
      ({:potentially_crashing_function, _}) -> true
      (_) -> false
    end)

  defp restricted_transformations_count(type), do:
    type.history_of_restricted_transformations
    |> Enum.filter(fn
      ({:restricted_function, _}) -> true
      (_) -> false
    end)
    |> Enum.count()

  defp filter_transformations(history_of_transformations, required_types), do:
    history_of_transformations
    |> Enum.filter(fn({type, _}) -> type in required_types end)
    |> Enum.group_by(fn({type, _}) -> type end, fn({_, name}) -> name end)
    |> Enum.map(fn({type, names}) -> {type, Enum.uniq(names)} end)
end
