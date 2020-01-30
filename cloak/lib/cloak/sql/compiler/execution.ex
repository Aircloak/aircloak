defmodule Cloak.Sql.Compiler.Execution do
  @moduledoc """
  Makes the compiled query specification ready for execution.

  This is strictly speaking a post-compilation step where we do some transformation
  to the query, in order to be able to safely execute it.
  """

  alias Cloak.Sql.{CompilationError, Condition, Expression, FixAlign, Function, Query, Range}
  alias Cloak.Sql.Compiler.{Helpers, Optimizer}
  alias Cloak.Sql.Query.Lenses

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Aligns the query parameters before execution."
  @spec align(Query.t()) :: Query.t()
  def align(%Query{command: :show} = query), do: query

  def align(query) do
    {info, query} =
      Query.Lenses.all_queries(analyst_tables?: false)
      |> Lens.get_and_map(query, fn subquery ->
        subquery = align_subquery(subquery)
        {subquery.info, subquery}
      end)

    %Query{query | info: Enum.concat(info)}
  end

  @doc "Prepares the query for execution."
  @spec prepare(Query.t()) :: Query.t()
  def prepare(%Query{command: :show} = query), do: query

  def prepare(%Query{command: :select} = query),
    do:
      query
      |> Helpers.apply_bottom_up(&censor_selected_uids/1, analyst_tables?: false)
      |> Helpers.apply_bottom_up(&reject_null_user_ids/1, analyst_tables?: false)
      |> Helpers.apply_bottom_up(&compute_aggregators/1, analyst_tables?: false)
      |> Helpers.apply_bottom_up(&expand_virtual_tables/1, analyst_tables?: false)
      |> Helpers.apply_bottom_up(&protect_against_join_timing_attacks/1, analyst_tables?: false)

  # -------------------------------------------------------------------
  # UID handling
  # -------------------------------------------------------------------

  defp reject_null_user_ids(%Query{type: :anonymized} = query) do
    user_id = %Expression{Helpers.id_column(query) | synthetic?: true}
    is_not_null = Expression.function("not", [Expression.function("is_null", [user_id], :boolean)], :boolean)

    %{query | where: Condition.both(is_not_null, query.where)}
  end

  defp reject_null_user_ids(query), do: query

  defp censor_selected_uids(%Query{type: :anonymized} = query) do
    # In an anonymized query, we're replacing all selected expressions which depend on uid columns with the `:*`
    # constant. This allows us to reduce the amount of anonymized values, without compromising the privacy.
    # For example, consider the query `select uid, name from users`. Normally, this would return only `(*, *)`
    # rows. However, with this replacement, we can return names which are frequent enough, without revealing
    # any sensitive information. For example, we could return: `(*, Alice), (*, Bob), (*, *)`, which is
    # not possible without this replacement.
    Lens.multiple([
      Lens.keys([:columns, :group_by]) |> Lens.all(),
      Lens.key(:order_by) |> Lens.all() |> Lens.at(0),
      Lens.key(:having) |> Lenses.conditions() |> Lenses.operands()
    ])
    |> Lens.filter(&non_aggregated_uid_expression?/1)
    |> Lens.map(query, &Expression.constant(&1.type, :*))
  end

  defp censor_selected_uids(query), do: query

  # returns true if the column contains an expression with non-aggregated user ids
  defp non_aggregated_uid_expression?(column),
    do:
      [column] |> get_in([Lenses.all_expressions()]) |> Enum.all?(&(not Function.aggregator?(&1))) and
        [column] |> extract_columns() |> Enum.any?(& &1.user_id?)

  # -------------------------------------------------------------------
  # Bucket alignment
  # -------------------------------------------------------------------

  defp align_buckets(%Query{type: :standard} = query), do: query

  defp align_buckets(query) do
    {messages, query} = Lens.get_and_map(Lenses.buckets(), query, &align_bucket/1)
    Query.add_info(query, Enum.reject(messages, &is_nil/1))
  end

  defp align_bucket(column) do
    if Function.bucket_size(column) <= 0 do
      raise CompilationError,
        source_location: column.source_location,
        message: "Bucket size #{Function.bucket_size(column)} must be > 0"
    end

    aligned = Function.update_bucket_size(column, &FixAlign.align/1)

    if aligned == column do
      {nil, aligned}
    else
      {"Bucket size adjusted from #{Function.bucket_size(column)} to #{Function.bucket_size(aligned)}", aligned}
    end
  end

  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp align_subquery(query) do
    query
    |> align_buckets()
    |> align_where()
    |> align_join_ranges()
    |> align_limit()
    |> align_offset()
    |> align_having()
  end

  @minimum_subquery_limit 10
  defp align_limit(query = %{limit: limit, type: :restricted}) when limit != nil do
    aligned = limit |> FixAlign.align() |> round() |> max(@minimum_subquery_limit)

    if aligned != limit do
      %{query | limit: aligned}
      |> Query.add_info("Limit adjusted from #{limit} to #{aligned}")
    else
      query
    end
  end

  defp align_limit(query), do: query

  defp align_offset(query = %{limit: limit, offset: offset, type: :restricted}) when offset != 0 do
    aligned = round(offset / limit) * limit

    if aligned != offset do
      %{query | offset: aligned}
      |> Query.add_info("Offset adjusted from #{offset} to #{aligned}")
    else
      query
    end
  end

  defp align_offset(query), do: query

  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp expand_arguments(%Expression{kind: :function} = column),
    do: [column | Enum.flat_map(column.args, &expand_arguments/1)]

  defp expand_arguments(column), do: [column]

  defp compute_aggregators(%Query{group_by: [_ | _]} = query),
    do: %Query{query | aggregators: aggregators(query)}

  defp compute_aggregators(query) do
    case aggregators(query) do
      [] -> %Query{query | aggregators: [Expression.count_star()], implicit_count?: true}
      aggregators -> %Query{query | aggregators: aggregators}
    end
  end

  defp aggregators(query),
    do:
      query
      |> Helpers.aggregator_sources()
      |> Enum.flat_map(&expand_arguments/1)
      |> Enum.filter(&Function.aggregator?/1)
      |> Enum.map(&Expression.semantic/1)
      |> Expression.unique()

  defp align_join_ranges(%Query{type: :standard} = query), do: query

  defp align_join_ranges(query),
    do:
      query
      |> Query.Lenses.join_condition_lenses()
      |> Enum.reduce(query, fn lens, query -> align_ranges(query, lens) end)

  defp align_having(%Query{type: :restricted} = query), do: align_ranges(query, Lens.key(:having))
  defp align_having(query), do: query

  defp align_where(%Query{type: :standard} = query), do: query
  defp align_where(query), do: align_ranges(query, Lens.key(:where))

  defp align_ranges(query, lens) do
    strip_inequalities = fn clause -> Condition.reject(clause, &Range.constant_inequality?/1) end
    stripped_query = Lens.map(lens, query, strip_inequalities)

    lens
    |> Range.inequalities_by_column(query)
    |> verify_ranges()
    |> Enum.reduce(stripped_query, &add_aligned_range(&1, &2, lens))
  end

  defp add_aligned_range({column, conditions}, query, lens) do
    {left, right} =
      conditions
      |> Enum.map(&Condition.value/1)
      |> Enum.sort(&Cloak.Data.lt_eq/2)
      |> List.to_tuple()
      |> FixAlign.align_interval()

    if implement_range?({left, right}, conditions) do
      [lhs, rhs] = conditions
      range = Condition.both(lhs, rhs)
      update_in(query, [lens], &Condition.both(range, &1))
    else
      query
      |> add_clause(lens, Expression.function("<", [column, Expression.constant(column.type, right)], :boolean))
      |> add_clause(lens, Expression.function(">=", [column, Expression.constant(column.type, left)], :boolean))
      |> Query.add_info(
        "The range for column #{Expression.display_name(column)} has been adjusted to #{left} <= " <>
          "#{Expression.short_name(column)} < #{right}."
      )
    end
  end

  defp implement_range?({left, right}, conditions) do
    [
      %Expression{kind: :function, name: left_operator, args: [_, left_column]},
      %Expression{kind: :function, name: right_operator, args: [_, right_column]}
    ] = Enum.sort_by(conditions, &Condition.value/1, &Cloak.Data.lt_eq/2)

    left_operator == ">=" and left_column.value == left and right_operator == "<" and right_column.value == right
  end

  defp add_clause(query, lens, clause), do: Lens.map(lens, query, &Condition.both(clause, &1))

  defp verify_ranges(grouped_inequalities) do
    grouped_inequalities
    |> Enum.reject(fn {_, comparisons} -> valid_range?(comparisons) end)
    |> case do
      [{_, [inequality | _]} | _] ->
        column = Condition.subject(inequality)

        raise CompilationError,
          source_location: column.source_location,
          message: "Column #{Expression.display_name(column)} must be limited to a finite, nonempty range."

      _ ->
        grouped_inequalities
    end
  end

  defp valid_range?(comparisons) do
    case Enum.sort_by(comparisons, &Condition.direction/1, &Kernel.>/2) do
      [cmp1, cmp2] ->
        Condition.direction(cmp1) != Condition.direction(cmp2) and
          Cloak.Data.lt(Condition.value(cmp1), Condition.value(cmp2))

      [cmp] ->
        cmp |> Condition.value() |> current_date?()

      _ ->
        false
    end
  end

  defp extract_columns(columns), do: Query.Lenses.leaf_expressions() |> Lens.to_list(columns)

  # -------------------------------------------------------------------
  # Virtual tables
  # -------------------------------------------------------------------

  defp expand_virtual_tables(query) do
    query.selected_tables
    |> Enum.reject(&(&1.query == nil))
    |> Enum.reduce(query, fn %{query: table_query, name: table_name}, query ->
      Lenses.leaf_tables()
      |> Lens.filter(&(&1 == table_name))
      |> Lens.map(query, &{:subquery, %{alias: &1, ast: table_query}})
    end)
    |> Optimizer.optimize_columns_from_subqueries()
  end

  # -------------------------------------------------------------------
  # Protect against join timing attacks
  #
  # For details, see `docs/anonymization.md`.
  # -------------------------------------------------------------------

  defp protect_against_join_timing_attacks(query), do: %Query{query | from: protect_joins(query.from)}

  defp protect_joins({:join, join}) do
    {lhs, rhs} =
      if join.type == :right_outer_join do
        {join.lhs, protect_join_branch(join.rhs)}
      else
        {protect_join_branch(join.lhs), join.rhs}
      end

    {:join, %{join | lhs: protect_joins(lhs), rhs: protect_joins(rhs)}}
  end

  defp protect_joins(query), do: query

  defp query_has_db_filters?(query) do
    Lenses.db_filter_clauses() |> Lens.reject(&is_nil/1) |> Lens.reject(&key_comparison?/1) |> Lens.to_list(query) != []
  end

  defp key_comparison?(%Expression{kind: :function, name: "=", args: [subject, target]}),
    do: Expression.key?(subject) and Expression.key?(target)

  defp key_comparison?(_), do: false

  defp query_needs_protection?(query), do: query.type == :restricted and query_has_db_filters?(query)

  defp protect_join_branch({:subquery, subquery}),
    do: {:subquery, Map.put(subquery, :join_timing_protection?, query_needs_protection?(subquery.ast))}

  defp protect_join_branch(branch), do: branch
end
