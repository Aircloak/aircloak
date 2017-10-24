defmodule Cloak.Sql.TypeChecker do
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

  alias Cloak.Sql.{CompilationError, Condition, Expression, Query}
  alias Cloak.Sql.TypeChecker.{Narrative, Type}


  # -------------------------------------------------------------------
  # Function and operator classifications
  # -------------------------------------------------------------------

  @discontinuous_math_functions ~w(% abs ceil ceiling div floor mod round trunc)
  @discontinuous_string_functions ~w(btrim left ltrim right rtrim substring)
  @continuous_math_functions ~w(+ - * / ^ pow)
  @datetime_functions ~w(year quarter month day hour minute second weekday)


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @spec validate_allowed_usage_of_math_and_functions(Query.t) :: Query.t
  def validate_allowed_usage_of_math_and_functions(query) do
    verify_usage_of_potentially_crashing_functions(query)
    verify_usage_of_datetime_extraction_clauses(query)
    verify_function_usage_for_selected_columns(query)
    verify_function_usage_for_condition_clauses(query)
    verify_lhs_of_in_is_clear(query)
    verify_lhs_of_not_equals_is_clear(query)
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
        explanation = type.narrative_breadcrumbs
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

  defp verify_usage_of_datetime_extraction_clauses(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.to_list(query)
    |> Enum.each(fn(comparison) ->
      types = Condition.targets(comparison)
      |> Enum.map(&establish_type(&1, query))
      |> Enum.uniq()
      if Enum.any?(types, & &1.is_result_of_datetime_processing?) and
          Enum.any?(types, & &1.constant? or &1.constant_involved?) do
        explanations = types
        |> Enum.filter(& &1.is_result_of_datetime_processing?)
        |> Enum.map_join(" ", fn(type) ->
          type.narrative_breadcrumbs
          |> reject_all_but_relevant_offensive_actions([:datetime_processing])
          |> Narrative.construct()
        end)
        raise CompilationError, message: """
          #{explanations}

          The results of functions that extract a component of a date, time, or datetime column
          cannot be used in filter conditions (like WHERE, HAVING and JOIN-conditions) when the
          value it is compared against is a constant or an expression involving a constant.

          If applicable, consider using a range on the native column instead.
          For example: column >= 'YYYY-MM-DD' and column < 'YYYY-MM-DD'.
          """
      end
    end)

  defp verify_function_usage_for_selected_columns(%Query{columns: _columns, subquery?: true} = query), do: query
  defp verify_function_usage_for_selected_columns(%Query{columns: columns} = query), do:
    Enum.each(columns, fn(column) ->
      type = establish_type(column, query)
      if type.dangerously_discontinuous? and type.seen_dangerous_math? do
        explanation = type.narrative_breadcrumbs
        |> filter_for_offensive_actions([:dangerously_discontinuous, :dangerous_math])
        |> reject_all_but_relevant_offensive_actions([:dangerously_discontinuous, :dangerous_math])
        |> Narrative.construct()
        raise CompilationError, message: """
          #{explanation}

          Queries where a reported value is influenced by math and a discontinuous function
          in conjunction with a constant are not allowed.
          """
      end
    end)

  defp verify_function_usage_for_condition_clauses(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Query.Lenses.order_condition_columns()
    |> Lens.to_list(query)
    |> Enum.each(fn(column) ->
      type = establish_type(column, query)
      if type.dangerously_discontinuous? or type.seen_dangerous_math? do
        explanation = type.narrative_breadcrumbs
        |> reject_all_but_relevant_offensive_actions([
          :dangerously_discontinuous,
          :dangerous_math
        ])
        |> Narrative.construct()
        raise CompilationError, message: """
          #{explanation}

          Inequality clauses used to filter the data (like WHERE, HAVING and JOIN-condition where >,
          >=, < or <= are used) are not allowed if the column value has either been transformed by
          a math function or a discontinuous function where one of the other parameters was a constant.
          """
      end
    end)

  defp verify_lhs_of_in_is_clear(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.in?/1)
    |> Lens.to_list(query)
    |> Enum.each(fn({:in, lhs, _}) ->
      unless establish_type(lhs, query).raw_column? do
        raise CompilationError, message: "The left-hand side of an IN operator must be an unmodified database column."
      end
    end)

  defp verify_lhs_of_not_equals_is_clear(query), do:
    Query.Lenses.db_filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.satisfy(&Condition.not_equals?/1)
    |> Lens.to_list(query)
    |> Enum.each(fn({:comparison, lhs, :<>, rhs}) ->
      unless establish_type(lhs, query).raw_column? and establish_type(rhs, query).constant? do
        raise CompilationError, message:
          "The <> operation can only be applied to an unmodified database column and a constant."
      end
    end)

  def establish_type(column, query), do: construct_type(column, query)


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

  defp performs_datetime_processing?(name, child_types), do:
    any_touched_by_datetime?(child_types) and (name in @datetime_functions or match?({:cast, _}, name))

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

  defp later_turned_into_a_number?(future), do:
    Enum.any?(["length", {:cast, :integer}, {:cast, :real}, {:cast, :boolean}], &(Enum.member?(future, &1)))

  defp any_touched_by_constant?(types), do: Enum.any?(types, &(&1.constant_involved?))

  defp any_touched_by_datetime?(types), do: Enum.any?(types, &(&1.datetime_involved?))

  defp constant(), do: %Type{constant?: true, constant_involved?: true}

  defp datetime_type?(:*), do: false
  defp datetime_type?(%Expression{type: type}), do: type in [:datetime, :date, :time]

  defp column(expression), do:
    %Type{
      raw_column?: true,
      constant?: false,
      narrative_breadcrumbs: [{expression, []}],
      datetime_involved?: datetime_type?(expression),
    }

  defp construct_type(column, query, future \\ [])
  defp construct_type(:null, _query, _future), do: constant()
  defp construct_type({:distinct, column}, query, future), do:
    construct_type(column, query, ["distinct" | future])
  defp construct_type(:*, _query, _future), do: column(:*)
  defp construct_type(%Expression{constant?: true}, _query, _future), do: constant()
  defp construct_type(%Expression{function: nil} = column, query, future), do:
    expand_from_subquery(column, query, future)
  defp construct_type(%Expression{function: name, function_args: args}, query, future), do:
    type_for_function(name, args, query, future)
  defp construct_type({:function, name, args}, query, future), do:
    type_for_function(name, args, query, future)

  defp type_for_function(name, args, query, future) do
    child_types = args |> Enum.map(&(construct_type(&1, query, [name | future])))
    # Prune constants, they don't interest us further
    if Enum.all?(child_types, &(&1.constant?)) do
      constant()
    else
      %Type{
        constant_involved?: any_touched_by_constant?(child_types),
        datetime_involved?: any_touched_by_datetime?(child_types),
        is_result_of_potentially_crashing_function?: performs_potentially_crashing_function?(name, child_types) ||
          Enum.any?(child_types, &(&1.is_result_of_potentially_crashing_function?)),
        seen_dangerous_math?: is_dangerous_math?(name, future, child_types) ||
          Enum.any?(child_types, &(&1.seen_dangerous_math?)),
        dangerously_discontinuous?: dangerously_discontinuous?(name, future, child_types) ||
          Enum.any?(child_types, &(&1.dangerously_discontinuous?)),
        narrative_breadcrumbs: extend_narrative_breadcrumbs(name, future, child_types),
        is_result_of_datetime_processing?: performs_datetime_processing?(name, child_types) ||
          Enum.any?(child_types, &(&1.is_result_of_datetime_processing?)),
      }
    end
  end

  defp extend_narrative_breadcrumbs(name, future, child_types), do:
    Narrative.extend(child_types, [
      {dangerously_discontinuous?(name, future, child_types), {:dangerously_discontinuous, name}},
      {is_dangerous_math?(name, future, child_types), {:dangerous_math, name}},
      {performs_datetime_processing?(name, child_types), {:datetime_processing, name}},
      {performs_potentially_crashing_function?(name, child_types),
        {:potentially_crashing_function, name}},
    ])

  defp expand_from_subquery(column, query, future) do
    Lens.to_list(Query.Lenses.direct_subqueries(), query)
    |> Enum.find(&(&1.alias == column.table.name))
    |> case do
      nil -> column(column)
      %{ast: subquery} ->
        column_index = Enum.find_index(subquery.column_titles, &(&1 == column.name))
        column = Enum.at(subquery.columns, column_index)
        construct_type(column, subquery, future)
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
end
