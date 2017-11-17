defmodule Cloak.Sql.Compiler.TypeChecker do
  @moduledoc """
  Provides functions to check whether the expressions used in a query are valid according to our anonymization rules.
  This includes checks to determine if the number of dangerous functions used (like functions that truncate values
  and could be used to create boolean logic circumventing other checks) exceed allowed thresholds, as well
  as checks to validate that columns used in certain filter conditions haven't been altered.
  """

  alias Cloak.Sql.{CompilationError, Condition, Function, Expression, Query, Range}
  alias Cloak.Sql.Compiler.TypeChecker.{Narrative, Type}
  alias Cloak.Sql.Compiler.Helpers

  @max_allowed_dangerous_functions 5
  @math_operations_before_considered_constant 1


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec validate_allowed_usage_of_math_and_functions(Query.t) :: Query.t
  def validate_allowed_usage_of_math_and_functions(query) do
    each_subquery(query, &verify_usage_of_potentially_crashing_functions/1)
    each_subquery(query, &verify_allowed_usage_of_math/1)
    each_subquery(query, &verify_lhs_of_in_is_clear/1)
    each_subquery(query, &verify_lhs_of_not_equals_is_clear/1)
    each_subquery(query, &verify_lhs_of_not_like_is_clear/1)
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
      type = establish_type(column, query)
      if potentially_crashing_function?(type) do
        explanation = type.history_of_dangerous_transformations
        |> offensive_transformations([:potentially_crashing_function])
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
      type = establish_type(expression, query)
      if dangerous_transformation_count(type) > @max_allowed_dangerous_functions do
        explanation = type.history_of_dangerous_transformations
        |> offensive_transformations([:dangerous_function])
        |> Narrative.construct(type.history_of_columns_involved)
        raise CompilationError, message: """
          #{explanation}

          Queries containing expressions with a high number of functions that are used in combination
          with constant values are prohibited. For further information about when this condition is
          triggered, please check the restrictions section of the user guides.
          """
      end
    end)

  @allowed_in_functions ~w(lower upper)
  defp verify_lhs_of_in_is_clear(query), do:
    verify_conditions(query, &Condition.in?/1, fn({:in, lhs, _}) ->
      unless clear_lhs?(lhs, query, @allowed_in_functions) do
        raise CompilationError, message:
          "Only #{function_list(@allowed_in_functions)} can be used in the left-hand side of an IN operator."
      end
    end)

  @allowed_not_equals_functions ~w(lower upper)
  defp verify_lhs_of_not_equals_is_clear(query), do:
    verify_conditions(query, &Condition.not_equals?/1, fn({:comparison, lhs, :<>, rhs}) ->
      unless clear_lhs?(lhs, query, @allowed_not_equals_functions), do:
        raise CompilationError, message:
          "Only #{function_list(@allowed_not_equals_functions)} can be used in the left-hand side of an <> operator."
      unless establish_type(rhs, query).constant?, do:
        raise CompilationError, message: "The right-hand side of an <> operator has to be a constant."
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
  defp clear_lhs?(%Expression{function: function, function_args: [lhs]}, query, allowed_functions), do:
    (function in allowed_functions) and clear_lhs?(lhs, query, allowed_functions)
  defp clear_lhs?(lhs, query, _allowed_functions), do: establish_type(lhs, query).raw_column?

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
  defp clear_range_lhs?(lhs, query, :implicit), do: establish_type(lhs, query).raw_implicit_range?
  defp clear_range_lhs?(lhs, query, _), do: establish_type(lhs, query).cast_raw_column?

  defp verify_conditions(query, predicate, action), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(predicate)
    |> Lens.to_list(query)
    |> Enum.each(action)


  # -------------------------------------------------------------------
  # Function classification
  # -------------------------------------------------------------------

  defp performs_potentially_crashing_function?("/", [_, child_type]), do:
    # This allows division by a pure constant, but not by a column influenced by a constant
    child_type.constant_involved? && not child_type.constant?
  defp performs_potentially_crashing_function?("sqrt", [child_type]), do:
    # This allows usage of square root on a pure constant, but not by a column influenced by a constant
    child_type.constant_involved? && not child_type.constant?
  defp performs_potentially_crashing_function?(_other, _child_type), do: false


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp each_subquery(query, function), do:
    Helpers.apply_bottom_up(query, fn(subquery) ->
      function.(subquery)
      subquery
    end)

  defp any_touched_by_constant?(types), do: Enum.any?(types, &(&1.constant_involved?))

  defp potentially_crashing_function?(type), do:
    Enum.any?(type.history_of_dangerous_transformations, fn
        ({:potentially_crashing_function, _}) -> true
        (_) -> false
      end)

  defp dangerous_transformation_count(type), do:
    type.history_of_dangerous_transformations
    |> Enum.filter(fn
      ({:dangerous_function, _}) -> true
      (_) -> false
    end)
    |> Enum.count()

  defp constant(), do: %Type{constant?: true, constant_involved?: true}

  defp column(expression), do:
    %Type{
      raw_column?: true,
      cast_raw_column?: true,
      constant?: false,
      history_of_columns_involved: [expression],
    }

  defp establish_type(column, query)
  defp establish_type(:null, _query), do: constant()
  defp establish_type({:distinct, column}, query), do: establish_type(column, query)
  defp establish_type(:*, _query), do: column(:*)
  defp establish_type(%Expression{constant?: true}, _query), do: constant()
  defp establish_type(%Expression{function: nil} = column, query), do: expand_from_subquery(column, query)
  defp establish_type(function = %Expression{function?: true}, query), do: type_for_function(function, query)

  defp type_for_function(function = %Expression{function: name, function_args: args}, query) do
    child_types = args |> Enum.map(&(establish_type(&1, query)))
    # Prune constants, they don't interest us further
    if Enum.all?(child_types, &(&1.constant?)) do
      constant()
    else
      applied_functions = [name | Enum.flat_map(child_types, & &1.applied_functions)]
      %Type{
        applied_functions: applied_functions,
        cast_raw_column?: Function.cast?(function) and match?([%{raw_column?: true}], child_types),
        raw_implicit_range?: Function.has_attribute?(function, :implicit_range) and
          Enum.all?(child_types, &(&1.cast_raw_column? || &1.constant?)),
        constant_involved?: any_touched_by_constant?(child_types) ||
          math_operations_count(applied_functions) > @math_operations_before_considered_constant,
        history_of_columns_involved: combined_columns_involved(child_types),
      }
      |> extend_history_of_dangerous_transformations(name, child_types)
    end
  end

  defp combined_columns_involved(child_types), do:
    child_types
    |> Enum.flat_map(& &1.history_of_columns_involved)
    |> Enum.uniq()

  defp extend_history_of_dangerous_transformations(%Type{constant_involved?: constant_involved?} = type,
      name, child_types) do
    full_history = [
      {constant_involved? && dangerous?(name), {:dangerous_function, name}},
      {performs_potentially_crashing_function?(name, child_types), {:potentially_crashing_function, name}},
    ]
    |> Enum.filter(fn({whether_applies, _}) -> whether_applies end)
    |> Enum.map(fn({_, history_element}) -> history_element end)
    |> Enum.concat(
      Enum.flat_map(child_types, & &1.history_of_dangerous_transformations)
    )
    %Type{type | history_of_dangerous_transformations: full_history}
  end

  defp dangerous?(name), do:
    Function.discontinuous_function?(name) or Function.math_function?(name)

  defp expand_from_subquery(column, query) do
    case Query.resolve_subquery_column(column, query) do
      :database_column -> column(column)
      {column, subquery} -> establish_type(column, subquery)
    end
  end

  defp offensive_transformations(history_of_transformations, required_types), do:
    history_of_transformations
    |> Enum.filter(fn({type, _}) -> type in required_types end)
    |> Enum.group_by(fn({type, _}) -> type end, fn({_, name}) -> name end)
    |> Enum.map(fn({type, names}) -> {type, Enum.uniq(names)} end)

  defp math_operations_count(applied_functions), do:
    applied_functions
    |> Enum.filter(& Function.math_function?(&1))
    |> Enum.count()


  # -------------------------------------------------------------------
  # Functions exposed for testing
  # -------------------------------------------------------------------

  if Mix.env == :test do
    @doc false
    def test_establish_type(column, query), do: establish_type(column, query)
  end
end
