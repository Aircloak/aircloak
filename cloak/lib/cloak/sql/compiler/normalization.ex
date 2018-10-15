defmodule Cloak.Sql.Compiler.Normalization do
  @moduledoc "Deals with normalizing some expressions so that they are easier to deal with at later stages."

  alias Cloak.Sql.Compiler.{Helpers, Normalization.Noops}
  alias Cloak.Sql.{Expression, Query, LikePattern, Condition, CompilationError}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Performs semantics preserving query transformations that should be done ahead of the
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
      |> Noops.remove()
      |> Helpers.apply_bottom_up(&normalize_constants/1)
      |> Helpers.apply_bottom_up(&normalize_comparisons/1)
      |> Helpers.apply_bottom_up(&normalize_order_by/1)

  @doc """
  Performs semantics preserving query transformations that cannot be done before validations
  have taken place as the validator (or other query compiler mechanics) rely on certain which
  the normalizer would rewrite and or remove.

  * Removes redundant occurences of "%" from LIKE patterns (for example "%%" -> "%")
  * Normalizes sequences of "%" and "_" in like patterns so that the "%" always precedes a sequence of "_"
  * Expands `BUCKET` calls into equivalent mathematical expressions

  These are useful (among others) for noise layers - we want to generate the same layer for semantically identical
  conditions, otherwise we have to fall back to probing.
  """
  @spec postvalidation_normalizations(Query.t()) :: Query.t()
  def postvalidation_normalizations(query),
    do:
      query
      |> Helpers.apply_bottom_up(&normalize_trivial_like/1)
      |> Helpers.apply_bottom_up(&normalize_bucket/1)
      |> Helpers.apply_bottom_up(&strip_source_location/1)

  # -------------------------------------------------------------------
  # Removing source location
  # -------------------------------------------------------------------

  defp strip_source_location(query), do: update_in(query, [Query.Lenses.query_expressions()], &Expression.semantic/1)

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

  defp do_normalize_constants(expression = %Expression{function: {:bucket, _}}), do: expression

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
  # Normalizing ORDER BY
  # -------------------------------------------------------------------

  defp normalize_order_by(query = %{subquery?: false}), do: query

  defp normalize_order_by(query) do
    case {query.order_by, remove_constant_ordering(query.order_by)} do
      {[], _} ->
        query

      {_, []} ->
        # We can't completely remove the `ORDER BY` clause here, because `LIMIT` and/or `OFFSET` might require it.
        %{query | order_by: [{Helpers.id_column(query), :asc, :nulls_last}]}

      {_, order_list} ->
        %{query | order_by: order_list}
    end
  end

  defp remove_constant_ordering(order_list),
    do: Enum.reject(order_list, fn {expression, _direction, _nulls} -> expression.constant? end)

  # -------------------------------------------------------------------
  # DISTINCT rewriting
  # -------------------------------------------------------------------

  defp rewrite_distinct(%Query{distinct?: true, group_by: [_ | _], order_by: [{column, _dir, _nulls} | _]} = query) do
    raise Cloak.Sql.CompilationError,
      source_location: column.source_location,
      message:
        "Simultaneous usage of DISTINCT, GROUP BY, and ORDER BY in the same query is not supported." <>
          " Try using a subquery instead."
  end

  defp rewrite_distinct(%Query{distinct?: true, group_by: [], columns: columns} = query) do
    if aggregate_query?(query) do
      %Query{query | distinct?: false}
    else
      %Query{query | distinct?: false, group_by: columns}
    end
  end

  defp rewrite_distinct(%Query{distinct?: true, columns: [column | _] = columns, group_by: [_ | _] = group_by} = query) do
    if Enum.all?(columns, &shallow_in(&1, group_by)) do
      functional_group_bys = Enum.filter(group_by, &shallow_in(&1, columns))
      %Query{query | distinct?: false, group_by: functional_group_bys}
    else
      query
    end
  end

  defp rewrite_distinct(ast), do: ast

  defp temp_rewrite_distinct(%Query{distinct?: true, group_by: [_ | _]} = query) do
    %Query{
      command: :select,
      # ... all columns from sub query (he previously used *
      columns: [],
      subquery?: query.subquery?
    }
  end

  defp aggregate_query?(query) do
    aggregate_expressions =
      Query.Lenses.query_expressions()
      |> Lens.filter(& &1.aggregate?)
      |> Lens.to_list(query)

    not Enum.empty?(aggregate_expressions)
  end

  defp shallow_in(expression, expressions),
    do: drop_location(expression) in Enum.map(expressions, &drop_location/1)

  defp drop_location(expression), do: Map.put(expression, :source_location, nil)
end
