defmodule Cloak.Sql.Compiler.Normalization do
  @moduledoc "Deals with normalizing some expressions so that they are easier to deal with at later stages."

  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.{Expression, Query, LikePattern, Condition, Function, CompilationError}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Performs semantics-preserving query transformations that should be done ahead of the
  query validation and before the noise layers and other anonymization properties are calculated.

  Performs rewrites such as:
  * Removing casts that cast a value to the same type it already is
  * Removing rounding/truncating of integers
  * Ensuring comparisons bewteen columns and constants have the form {column, operator, constant}
  * Removing grouping by constant (column number references should have been compiled at this point)
  """
  @spec prevalidation_normalizations(Query.t()) :: Query.t()
  def prevalidation_normalizations(query),
    do:
      query
      |> Helpers.apply_bottom_up(&rewrite_distinct/1)
      |> Helpers.apply_bottom_up(&remove_redundant_casts/1)
      |> Helpers.apply_bottom_up(&remove_redundant_rounds/1)
      |> Helpers.apply_bottom_up(&remove_conditionless_cases/1)
      |> Helpers.apply_bottom_up(&normalize_non_anonymizing_noise/1)
      |> Helpers.apply_bottom_up(&normalize_constants/1)
      |> Helpers.apply_bottom_up(&normalize_comparisons/1)
      |> Helpers.apply_bottom_up(&normalize_order_by/1)
      |> Helpers.apply_bottom_up(&make_boolean_comparisons_explicit/1)

  @doc """
  Performs semantics-preserving query transformations that remove query properties needed by the validator
  (or other query compiler mechanics) and therefore cannot be done earlier.

  * Removes redundant occurences of "%" from LIKE patterns (for example "%%" -> "%")
  * Normalizes sequences of "%" and "_" in like patterns so that the "%" always precedes a sequence of "_"
  * Expands `BUCKET` calls into equivalent mathematical expressions
  * Expands `AVG` calls into `SUM/COUNT` calls.
  """
  @spec postvalidation_normalizations(Query.t()) :: Query.t()
  def postvalidation_normalizations(query),
    do:
      query
      |> Helpers.apply_bottom_up(&normalize_trivial_like/1, analyst_tables?: false)
      |> Helpers.apply_bottom_up(&normalize_bucket/1, analyst_tables?: false)
      |> Helpers.apply_bottom_up(&normalize_anonymizing_aggregators/1, analyst_tables?: false)
      |> Helpers.apply_bottom_up(&strip_source_location/1, analyst_tables?: false)

  # -------------------------------------------------------------------
  # Removing source location
  # -------------------------------------------------------------------

  defp strip_source_location(query), do: update_in(query, [Query.Lenses.query_expressions()], &Expression.semantic/1)

  # -------------------------------------------------------------------
  # Removing useless casts
  # -------------------------------------------------------------------

  defp remove_redundant_casts(query),
    do:
      update_in(query, [Query.Lenses.terminals()], fn
        %Expression{kind: :function, name: {:cast, type}, args: [expr = %Expression{type: type}]} ->
          expr

        other ->
          other
      end)

  # -------------------------------------------------------------------
  # Removing useless round/trunc
  # -------------------------------------------------------------------

  @round_funcs ~w/round trunc ceil floor/
  defp remove_redundant_rounds(query),
    do:
      update_in(query, [Query.Lenses.terminals()], fn
        %Expression{kind: :function, name: name, args: [expr = %Expression{type: :integer}]}
        when name in @round_funcs ->
          expr

        other ->
          other
      end)

  # -------------------------------------------------------------------
  # Normalizing comparisons
  # -------------------------------------------------------------------

  defp normalize_comparisons(query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Function.condition?/1)
    |> Lens.filter(&(Condition.verb(&1) == :comparison))
    |> Lens.map(query, &do_normalize_comparison/1)
  end

  defp do_normalize_comparison(%Expression{kind: :function, name: operator, args: [lhs, rhs]} = comparison) do
    if Expression.constant?(lhs) and not Expression.constant?(rhs),
      do: %Expression{comparison | name: invert_inequality(operator), args: [rhs, lhs]},
      else: comparison
  end

  defp invert_inequality("="), do: "="
  defp invert_inequality("<>"), do: "<>"
  defp invert_inequality(">"), do: "<"
  defp invert_inequality(">="), do: "<="
  defp invert_inequality("<"), do: ">"
  defp invert_inequality("<="), do: ">="

  # -------------------------------------------------------------------
  # Making boolean comparisons explicit
  # -------------------------------------------------------------------

  defp make_boolean_comparisons_explicit(query) do
    Query.Lenses.filter_clauses()
    |> Query.Lenses.conditions()
    |> Lens.reject(&Function.condition?/1)
    |> Lens.map(query, &do_make_boolean_comparison_explicit/1)
  end

  defp do_make_boolean_comparison_explicit(%Expression{
         kind: :function,
         name: "not",
         args: [%Expression{type: :boolean} = expression]
       }),
       do: Expression.function("=", [expression, Expression.constant(:boolean, false)], :boolean)

  defp do_make_boolean_comparison_explicit(%Expression{type: :boolean} = expression),
    do: Expression.function("=", [expression, Expression.constant(:boolean, true)], :boolean)

  # -------------------------------------------------------------------
  # Collapsing constant expressions
  # -------------------------------------------------------------------

  defp normalize_constants(query),
    do:
      Query.Lenses.terminals()
      |> Lens.filter(&Expression.constant?/1)
      |> Lens.filter(&Expression.function?/1)
      |> Lens.map(query, &do_normalize_constants/1)

  defp do_normalize_constants(expression) do
    case {Expression.const_value(expression), Enum.any?(expression.args, &is_nil(&1.value))} do
      {nil, true} ->
        Expression.null()

      {nil, false} ->
        raise CompilationError,
          source_location: expression.source_location,
          message: "Failed to evaluate expression `#{Expression.display(expression)}`."

      {value, _} ->
        Expression.constant(expression.type, value, expression.parameter_index)
        |> Expression.set_location(expression.source_location)
        |> put_in([Lens.key(:alias)], expression.alias)
    end
  end

  # -------------------------------------------------------------------
  # Normalizing like patterns
  # -------------------------------------------------------------------

  defp normalize_trivial_like(query),
    do:
      Query.Lenses.top_down_query_expressions()
      |> Lens.filter(&Function.condition?/1)
      |> Lens.filter(&(Condition.verb(&1) == :like))
      |> Lens.filter(&(&1 |> Condition.value() |> LikePattern.trivial?()))
      |> Lens.map(query, &do_normalize_trivial_like/1)

  defp do_normalize_trivial_like(%Expression{kind: :function, name: "not", args: [expression]}) do
    %Expression{do_normalize_trivial_like(expression) | name: "<>"}
  end

  defp do_normalize_trivial_like(%Expression{kind: :function, name: "like", args: [subject, target]} = expression) do
    %Expression{expression | name: "=", args: [subject, LikePattern.trivial_to_string(target)]}
  end

  defp do_normalize_trivial_like(%Expression{kind: :function, name: "ilike", args: [subject, target]} = expression) do
    args = [Expression.lowercase(subject), target |> Expression.lowercase() |> LikePattern.trivial_to_string()]
    %Expression{expression | name: "=", args: args}
  end

  # -------------------------------------------------------------------
  # Normalizing bucket calls
  # -------------------------------------------------------------------

  defp normalize_bucket(query),
    do: Lens.map(Query.Lenses.buckets(), query, &expand_bucket(&1.name, &1.args))

  defp expand_bucket({:bucket, :lower}, [arg1, arg2]),
    # floor(arg1 / arg2) * arg2
    do:
      Expression.function(
        "*",
        [
          arg2,
          Expression.function(
            "floor",
            [
              Expression.function("*", [arg1, inverse_constant(arg2)], :real)
            ],
            :integer
          )
        ],
        :real
      )

  defp expand_bucket({:bucket, :upper}, [arg1, arg2]),
    # floor(arg1 / arg2) * arg2 + arg2
    do:
      Expression.function(
        "+",
        [
          arg2,
          expand_bucket({:bucket, :lower}, [arg1, arg2])
        ],
        :real
      )

  defp expand_bucket({:bucket, :middle}, [arg1, arg2]),
    # floor(arg1 / arg2) * arg2 + 0.5 * arg2
    do:
      Expression.function(
        "+",
        [
          Expression.function("*", [Expression.constant(:real, 0.5), arg2], :real),
          expand_bucket({:bucket, :lower}, [arg1, arg2])
        ],
        :real
      )

  defp inverse_constant(%Expression{kind: :constant, value: value}) when is_number(value),
    do: Expression.constant(:real, 1.0 / value)

  # -------------------------------------------------------------------
  # Normalizing anonymized aggregators calls
  # -------------------------------------------------------------------

  defp normalize_anonymizing_aggregators(%Query{type: :anonymized} = query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name in ["stddev", "stddev_noise", "avg", "avg_noise"]))
    |> Lens.map(query, &normalize_anonymizing_aggregator/1)
  end

  defp normalize_anonymizing_aggregators(query), do: query

  defp normalize_anonymizing_aggregator(%Expression{name: "stddev"} = stddev_expression) do
    # `sd(v) = sqrt(variance(v))`
    Expression.function("sqrt", [%Expression{stddev_expression | name: "variance"}], :real)
  end

  defp normalize_anonymizing_aggregator(%Expression{name: "stddev_noise"} = stddev_expression) do
    # `sd_noise(v) = sqrt(variance_noise(v))`
    Expression.function("sqrt", [%Expression{stddev_expression | name: "variance_noise"}], :real)
  end

  defp normalize_anonymizing_aggregator(%Expression{args: [{:distinct, _arg}]} = expression), do: expression

  defp normalize_anonymizing_aggregator(%Expression{name: "avg", args: [arg]}),
    do:
      Expression.function(
        "/",
        [
          Expression.function("sum", [arg], :real),
          Expression.function("count", [arg], :integer)
        ],
        :real
      )

  defp normalize_anonymizing_aggregator(%Expression{name: "avg_noise", args: [arg]}),
    do:
      Expression.function(
        "/",
        [
          Expression.function("sum_noise", [arg], :real),
          Expression.function("count", [arg], :integer)
        ],
        :real
      )

  # -------------------------------------------------------------------
  # Normalizing non-anonymized noise calls
  # -------------------------------------------------------------------

  @noise_functions ~w(count_noise sum_noise avg_noise stddev_noise variance_noise)
  defp normalize_non_anonymizing_noise(%Query{type: type} = query) when type in [:standard, :restricted] do
    Query.Lenses.query_expressions()
    |> Lens.filter(&Expression.function?/1)
    |> Lens.filter(&(&1.name in @noise_functions))
    |> Lens.to_list(query)
    |> Enum.count()
    |> case do
      0 ->
        query

      _ ->
        Query.Lenses.query_expressions()
        |> Lens.filter(&Expression.function?/1)
        |> Lens.filter(&(&1.name in @noise_functions))
        |> Lens.map(query, fn _ -> Expression.constant(:real, 0.0) end)
        |> group_global_aggregators()
    end
  end

  defp normalize_non_anonymizing_noise(query), do: query

  defp group_global_aggregators(%Query{group_by: [], grouping_sets: []} = query),
    do: %Query{query | group_by: [], grouping_sets: [[]]}

  defp group_global_aggregators(query), do: query

  # -------------------------------------------------------------------
  # Normalizing ORDER BY
  # -------------------------------------------------------------------

  defp normalize_order_by(query = %{subquery?: false}), do: query

  defp normalize_order_by(query) do
    case {query.order_by, remove_constant_ordering(query.order_by), Helpers.id_column(query)} do
      {[], _, _} ->
        query

      {_, [], nil} ->
        # The query doesn't have a uid, so it will be processed in the cloak - we can remove the order_by
        %{query | order_by: []}

      {_, [], id_column} ->
        # We can't completely remove the `ORDER BY` clause here, because `LIMIT` and/or `OFFSET` might require it.
        %{query | order_by: [{id_column, :asc, :nulls_natural}]}

      {_, order_list, _} ->
        %{query | order_by: order_list}
    end
  end

  defp remove_constant_ordering(order_list),
    do: Enum.reject(order_list, fn {expression, _direction, _nulls} -> Expression.constant?(expression) end)

  # -------------------------------------------------------------------
  # DISTINCT rewriting
  # -------------------------------------------------------------------

  defp rewrite_distinct(%Query{distinct?: true, group_by: [_ | _], order_by: [{column, _dir, _nulls} | _]}),
    # Correctly rewriting a query that combines DISTINCT with GROUP BY and an ORDER BY is non-trivial.
    # See the following discussion for further details:
    # https://github.com/Aircloak/aircloak/pull/2864#pullrequestreview-135001173
    do:
      raise(Cloak.Sql.CompilationError,
        source_location: column.source_location,
        message:
          "Simultaneous usage of `DISTINCT`, `GROUP BY`, and `ORDER BY` in the same query is not supported." <>
            " Try using a subquery instead."
      )

  defp rewrite_distinct(%Query{distinct?: true, group_by: [], columns: columns} = query) do
    if Helpers.aggregates?(query) do
      %Query{query | distinct?: false}
    else
      columns = columns |> Enum.reject(&Expression.constant?/1)
      %Query{query | distinct?: false, group_by: columns, grouping_sets: Helpers.default_grouping_sets(columns)}
    end
  end

  defp rewrite_distinct(%Query{distinct?: true, columns: [_ | _] = columns, group_by: [_ | _] = group_by} = query) do
    cond do
      # - SELECT DISTINCT a, b FROM table GROUP BY a, b
      # - SELECT DISTINCT a FROM table GROUP BY a, b
      all_non_aggregates_grouped_by?(query) and not Helpers.aggregates?(query) ->
        functional_group_bys = Enum.filter(group_by, &Expression.member?(columns, &1))

        %Query{
          query
          | distinct?: false,
            group_by: functional_group_bys,
            grouping_sets: Helpers.default_grouping_sets(functional_group_bys)
        }

      # - SELECT DISTINCT a, count(*) FROM table GROUP BY a
      # - SELECT DISTINCT count(*) FROM table
      all_non_aggregates_grouped_by?(query) and Helpers.aggregates?(query) and not any_unselected_group_bys?(query) ->
        %Query{query | distinct?: false}

      # Currently not handled because it requires a complex subquery rewrite:
      # - SELECT DISTINCT a, count(*) FROM table GROUP BY a, b
      Helpers.aggregates?(query) and any_unselected_group_bys?(query) ->
        reject_unselected_group_by(query)

      # These can't be transformed correctly because the query is illegal
      # - SELECT DISTINCT a, b FROM table GROUP BY a
      true ->
        query
    end
  end

  defp rewrite_distinct(query), do: query

  defp reject_unselected_group_by(query) do
    [column | _] = Helpers.non_selected_group_bys(query)

    raise Cloak.Sql.CompilationError,
      source_location: column.source_location,
      message:
        "Grouping by unselected columns while using `DISTINCT` is not supported." <>
          " Try removing #{Expression.display_name(column)} from the `GROUP BY` clause"
  end

  defp any_unselected_group_bys?(query),
    do:
      query
      |> Helpers.non_selected_group_bys()
      |> Enum.count() > 0

  defp all_non_aggregates_grouped_by?(%Query{columns: columns, group_by: group_bys}),
    do:
      columns
      |> Enum.reject(&Helpers.aggregated_column?/1)
      |> Enum.all?(&Expression.member?(group_bys, &1))

  # -------------------------------------------------------------------
  # Removing conditionless cases
  # -------------------------------------------------------------------

  defp remove_conditionless_cases(query),
    do:
      update_in(query, [Query.Lenses.terminals()], fn
        %Expression{kind: :function, name: "case", args: [expr]} ->
          expr

        other ->
          other
      end)
end
