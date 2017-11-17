defmodule Cloak.Sql.Compiler.TypeChecker do
  @moduledoc """
  Provides functions to check whether selected columns, or expressions
  used in WHERE-clause inequalities have undergone dangerous transformations.

  What transformations are considered dangerous differs based on where
  an expression is to be used. WHERE-clause inequalities have stricter rules
  applied to them than selected columns.

  In particular this module considers whether a column has seen math
  or has had discontinuous functions applied to it. We currently only
  consider math and discontinuous functions dangerous if the inputs
  to a function have been tainted by an analyst-provided constant.
  Hence for example `abs(<pure-column>)` is ok, whereas
  `abs(<pure-column> + <constant>)` is not.
  Likewise `<pure-column> + <pure-column>` is ok, whereas
  `<pure-column> + (<pure-column> + <constant>)` is not.

  Note also that `<constant> MATH <constant>` and `DISCONTINUOUS_FUNCTION(<constant>)`
  are transformed to `<constant>`, and hence not considered as applications of math
  or discontinuous functions.
  """

  alias Cloak.Sql.{CompilationError, Condition, Function, Expression, Query, Range}
  alias Cloak.Sql.Compiler.TypeChecker.{Narrative, Type}
  alias Cloak.Sql.Compiler.Helpers


  # -------------------------------------------------------------------
  # Function and operator classifications
  # -------------------------------------------------------------------

  @discontinuous_math_functions ~w(% abs ceil ceiling div floor mod round trunc)
  @discontinuous_string_functions ~w(btrim left ltrim right rtrim substring)
  @continuous_math_functions ~w(+ - * / ^ pow)


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec validate_allowed_usage_of_math_and_functions(Query.t) :: Query.t
  def validate_allowed_usage_of_math_and_functions(query) do
    each_subquery(query, &verify_usage_of_potentially_crashing_functions/1)
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
      if type.is_result_of_potentially_crashing_function? do
        explanation = type.history_of_dangerous_transformations
        |> filter_for_offensive_actions([:potentially_crashing_function])
        |> reject_all_but_relevant_offensive_actions([:potentially_crashing_function])
        |> Narrative.construct()
        raise CompilationError, message: """
          #{explanation}

          Functions are not allowed to be used in ways that could cause a database exception
          to be raised in a way controllable by the analyst.
          For example this situation arises when a column or constant value is divided (`/`)
          by an expression that both contains a user data column as well as a constant value
          (for example `age / (age - 20)`), or if the square root is taken of an expression that
          contains a user data column as well as a constant value (for example `sqrt(age - 20)`).
          """
      end
    end)

  defp verify_lhs_of_in_is_clear(query), do:
    verify_conditions(query, &Condition.in?/1, fn({:in, lhs, _}) ->
      unless clear_lhs?(lhs, query) do
        raise CompilationError, message: "The left-hand side of an IN operator must be an unmodified database column."
      end
    end)

  defp verify_lhs_of_not_equals_is_clear(query), do:
    verify_conditions(query, &Condition.not_equals?/1, fn({:comparison, lhs, :<>, rhs}) ->
      unless clear_lhs?(lhs, query) and establish_type(rhs, query).constant? do
        raise CompilationError, message:
          "The <> operation can only be applied to an unmodified database column and a constant."
      end
    end)

  defp verify_lhs_of_not_like_is_clear(query), do:
    verify_conditions(query, &Condition.not_like?/1, fn({:not, {kind, lhs, _}}) ->
      unless clear_lhs?(lhs, query) do
        raise CompilationError, message:
          "NOT #{like_kind_name(kind)} can only be applied to an unmodified database column."
      end
    end)

  defp clear_lhs?(%Expression{aggregate?: true, function_args: [lhs]}, query), do: clear_lhs?(lhs, query)
  defp clear_lhs?(lhs, query), do: establish_type(lhs, query).raw_column?

  defp like_kind_name(:like), do: "LIKE"
  defp like_kind_name(:ilike), do: "ILIKE"

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

  defp dangerously_discontinuous?({:bucket, _}, _future, child_types), do:
    any_touched_by_constant?(child_types)
  defp dangerously_discontinuous?(name, _future, child_types)
      when name in @discontinuous_math_functions, do:
    any_touched_by_constant?(child_types)
  defp dangerously_discontinuous?({:cast, _}, _future, child_types), do: any_touched_by_constant?(child_types)
  defp dangerously_discontinuous?(name, future, child_types)
      when name in @discontinuous_string_functions, do:
    any_touched_by_constant?(child_types) and later_turned_into_a_number?(future)
  defp dangerously_discontinuous?(_name, _future, _child_types), do: false

  defp is_dangerous_math?(name, _future, child_types) when name in @continuous_math_functions, do:
    any_touched_by_constant?(child_types)
  defp is_dangerous_math?(_, _future, _child_types), do: false

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

  defp later_turned_into_a_number?(future), do:
    Enum.any?(["length", {:cast, :integer}, {:cast, :real}, {:cast, :boolean}], &(Enum.member?(future, &1)))

  defp any_touched_by_constant?(types), do: Enum.any?(types, &(&1.constant_involved?))

  defp constant(), do: %Type{constant?: true, constant_involved?: true}

  defp column(expression), do:
    %Type{
      raw_column?: true,
      cast_raw_column?: true,
      constant?: false,
      history_of_dangerous_transformations: [{expression, []}],
    }

  defp establish_type(column, query, future \\ [])
  defp establish_type(:null, _query, _future), do: constant()
  defp establish_type({:distinct, column}, query, future), do:
    establish_type(column, query, ["distinct" | future])
  defp establish_type(:*, _query, _future), do: column(:*)
  defp establish_type(%Expression{constant?: true}, _query, _future), do: constant()
  defp establish_type(%Expression{function: nil} = column, query, future), do:
    expand_from_subquery(column, query, future)
  defp establish_type(function = %Expression{function?: true}, query, future), do:
    type_for_function(function, query, future)

  defp type_for_function(function = %Expression{function: name, function_args: args}, query, future) do
    child_types = args |> Enum.map(&(establish_type(&1, query, [name | future])))
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
          math_operations_count(applied_functions) >= 2,
        is_result_of_potentially_crashing_function?: performs_potentially_crashing_function?(name, child_types) ||
          Enum.any?(child_types, &(&1.is_result_of_potentially_crashing_function?)),
        seen_dangerous_math?: is_dangerous_math?(name, future, child_types) ||
          Enum.any?(child_types, &(&1.seen_dangerous_math?)),
        dangerously_discontinuous?: dangerously_discontinuous?(name, future, child_types) ||
          Enum.any?(child_types, &(&1.dangerously_discontinuous?)),
        history_of_dangerous_transformations: extend_history_of_dangerous_transformations(name, future, child_types),
      }
    end
  end

  defp extend_history_of_dangerous_transformations(name, future, child_types), do:
    Narrative.extend(child_types, [
      {dangerously_discontinuous?(name, future, child_types), {:dangerously_discontinuous, name}},
      {is_dangerous_math?(name, future, child_types), {:dangerous_math, name}},
      {performs_potentially_crashing_function?(name, child_types),
        {:potentially_crashing_function, name}},
    ])

  defp expand_from_subquery(column, query, future) do
    case Query.resolve_subquery_column(column, query) do
      :database_column -> column(column)
      {column, subquery} -> establish_type(column, subquery, future)
    end
  end

  # Removes columns that haven't had all of a list of offenses applied to them
  defp filter_for_offensive_actions(columns, required_offenses), do:
    columns
    |> Enum.filter(fn({_expression, offenses}) ->
      offenses
      |> Enum.map(fn({name, _}) -> name end)
      |> Enum.reduce(required_offenses, fn (name, offenses) ->
        Enum.reject(offenses, & &1 == name)
      end) == []
    end)

  defp reject_all_but_relevant_offensive_actions(columns, relevant_offenses), do:
    Enum.map(columns, fn({expression, offenses}) ->
      {expression, Enum.filter(offenses, fn({offense_name, _}) ->
        Enum.member?(relevant_offenses, offense_name)
      end)}
    end)

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
