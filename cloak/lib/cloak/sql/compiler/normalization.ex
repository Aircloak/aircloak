defmodule Cloak.Sql.Compiler.Normalization do
  @moduledoc "Deals with normalizing some expressions so that they are easier to deal with at later stages."

  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.{Expression, Query, LikePattern, Condition, CompilationError}

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
  """
  @spec prevalidation_normalizations(Query.t()) :: Query.t()
  def prevalidation_normalizations(query),
    do:
      query
      |> Helpers.apply_bottom_up(&rewrite_distinct/1)
      |> Helpers.apply_bottom_up(&remove_redundant_casts/1)
      |> Helpers.apply_bottom_up(&remove_redundant_rounds/1)
      |> Helpers.apply_bottom_up(&normalize_constants/1)
      |> Helpers.apply_bottom_up(&normalize_comparisons/1)
      |> Helpers.apply_bottom_up(&normalize_order_by/1)

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
      |> Helpers.apply_bottom_up(&normalize_trivial_like/1)
      |> Helpers.apply_bottom_up(&normalize_bucket/1)
      |> Helpers.apply_bottom_up(&normalize_anonymized_avg/1)
      |> Helpers.apply_bottom_up(&strip_source_location/1)

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
        %Expression{function: {:cast, type}, function_args: [expr = %Expression{type: type}]} ->
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
        %Expression{function: fun, function_args: [expr = %Expression{type: :integer}]}
        when fun in @round_funcs ->
          expr

        other ->
          other
      end)

  # -------------------------------------------------------------------
  # Normalizing comparisons
  # -------------------------------------------------------------------

  defp normalize_comparisons(query),
    do:
      update_in(
        query,
        [
          Query.Lenses.filter_clauses()
          |> Query.Lenses.conditions()
          |> Lens.satisfy(&(Condition.verb(&1) == :comparison))
        ],
        &do_normalize_comparison/1
      )

  defp do_normalize_comparison({:not, condition}), do: {:not, do_normalize_comparison(condition)}

  defp do_normalize_comparison({:comparison, lhs, operator, rhs} = comparison),
    do:
      if(
        Expression.constant?(lhs) and not Expression.constant?(rhs),
        do: {:comparison, rhs, invert_inequality(operator), lhs},
        else: comparison
      )

  defp invert_inequality(:=), do: :=
  defp invert_inequality(:<>), do: :<>
  defp invert_inequality(:>), do: :<
  defp invert_inequality(:>=), do: :<=
  defp invert_inequality(:<), do: :>
  defp invert_inequality(:<=), do: :>=

  # -------------------------------------------------------------------
  # Collapsing constant expressions
  # -------------------------------------------------------------------

  defp normalize_constants(query),
    do: update_in(query, [Query.Lenses.terminals() |> Lens.filter(&Expression.constant?/1)], &do_normalize_constants/1)

  defp do_normalize_constants(expression = %Expression{function?: true, aggregate?: false}) do
    case Expression.const_value(expression) do
      nil ->
        raise CompilationError,
          source_location: expression.source_location,
          message: "Failed to evaluate expression `#{Expression.display(expression)}`."

      value ->
        Expression.constant(expression.type, value, expression.parameter_index)
        |> Expression.set_location(expression.source_location)
        |> put_in([Lens.key(:alias)], expression.alias)
    end
  end

  defp do_normalize_constants(other), do: other

  # -------------------------------------------------------------------
  # Normalizing like patterns
  # -------------------------------------------------------------------

  defp normalize_trivial_like(query),
    do:
      Query.Lenses.like_clauses()
      |> Lens.filter(&trivial_like?/1)
      |> Lens.map(query, fn
        {:like, lhs, rhs} ->
          {:comparison, lhs, :=, LikePattern.trivial_to_string(rhs)}

        {:ilike, lhs, rhs} ->
          {:comparison, Expression.lowercase(lhs), :=, rhs |> Expression.lowercase() |> LikePattern.trivial_to_string()}

        {:not, {:like, lhs, rhs}} ->
          {:comparison, lhs, :<>, LikePattern.trivial_to_string(rhs)}

        {:not, {:ilike, lhs, rhs}} ->
          {:comparison, Expression.lowercase(lhs), :<>,
           rhs |> Expression.lowercase() |> LikePattern.trivial_to_string()}
      end)

  defp trivial_like?({:not, like}), do: trivial_like?(like)
  defp trivial_like?({_kind, _rhs, lhs}), do: LikePattern.trivial?(lhs.value)

  # -------------------------------------------------------------------
  # Normalizing bucket calls
  # -------------------------------------------------------------------

  defp normalize_bucket(query),
    do: Lens.map(Query.Lenses.buckets(), query, &expand_bucket(&1.function, &1.function_args))

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
              Expression.function("/", [arg1, arg2], :real)
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

  # -------------------------------------------------------------------
  # Normalizing anonymized avg calls
  # -------------------------------------------------------------------

  defp normalize_anonymized_avg(%Query{type: :anonymized} = query) do
    Query.Lenses.query_expressions()
    |> Lens.filter(&(&1.function in ["avg", "avg_noise"]))
    |> Lens.reject(&row_splitter_expression?/1)
    |> Lens.map(query, &expand_avg/1)
  end

  defp normalize_anonymized_avg(query), do: query

  defp row_splitter_expression?({:distinct, expression}), do: row_splitter_expression?(expression)

  defp row_splitter_expression?(expression) do
    Expression.row_splitter?(expression) or Enum.any?(expression.function_args, &row_splitter_expression?/1)
  end

  defp expand_avg(%Expression{function: "avg", function_args: [arg]}),
    do:
      Expression.function(
        "/",
        [
          Expression.function("sum", [arg], :real, true),
          Expression.function("count", [arg], :integer, true)
        ],
        :real
      )

  defp expand_avg(%Expression{function: "avg_noise", function_args: [arg]}),
    do:
      Expression.function(
        "/",
        [
          Expression.function("sum_noise", [arg], :real, true),
          Expression.function("count", [arg], :integer, true)
        ],
        :real
      )

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
        %{query | order_by: [{id_column, :asc, :nulls_last}]}

      {_, order_list, _} ->
        %{query | order_by: order_list}
    end
  end

  defp remove_constant_ordering(order_list),
    do: Enum.reject(order_list, fn {expression, _direction, _nulls} -> expression.constant? end)

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
      %Query{query | distinct?: false, group_by: columns}
    end
  end

  defp rewrite_distinct(%Query{distinct?: true, columns: [_ | _] = columns, group_by: [_ | _] = group_by} = query) do
    cond do
      # - SELECT DISTINCT a, b FROM table GROUP a, b
      # - SELECT DISTINCT a FROM table GROUP a, b
      all_non_aggregates_grouped_by?(query) and not Helpers.aggregates?(query) ->
        functional_group_bys = Enum.filter(group_by, &Expression.member?(columns, &1))
        %Query{query | distinct?: false, group_by: functional_group_bys}

      # - SELECT DISTINCT a, count(*) FROM table GROUP a
      # - SELECT DISTINCT count(*) FROM table
      all_non_aggregates_grouped_by?(query) and Helpers.aggregates?(query) and not any_unselected_group_bys?(query) ->
        %Query{query | distinct?: false}

      # Currently not handled because it requires a complex subquery rewrite:
      # - SELECT DISTINCT a, count(*) FROM table GROUP a, b
      Helpers.aggregates?(query) and any_unselected_group_bys?(query) ->
        reject_unselected_group_by(query)

      # These can't be transformed correctly because the query is illegal
      # - SELECT DISTINCT a, b FROM table GROUP a
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
end
