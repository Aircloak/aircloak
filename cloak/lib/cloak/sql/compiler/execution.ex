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

  defp add_aligned_range({column, [condition]}, query, lens) do
    target = Condition.value(condition)
    truncated_target = truncate_datetime(target)

    if target == truncated_target do
      update_in(query, [lens], &Condition.both(condition, &1))
    else
      query
      |> add_clause(lens, %Expression{condition | args: [column, Expression.constant(column.type, truncated_target)]})
      |> Query.add_info(
        "The inequality target for column #{Expression.display_name(column)} has been adjusted to `#{truncated_target}`."
      )
    end
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
        [_subject, target1] = Condition.targets(cmp1)
        [_subject, target2] = Condition.targets(cmp2)

        Condition.direction(cmp1) != Condition.direction(cmp2) and
          compatible_types?(target1.type, target2.type) and
          Cloak.Data.lt(Expression.const_value(target1), Expression.const_value(target2))

      [cmp] ->
        cmp |> Condition.value() |> current_date?()

      _ ->
        false
    end
  end

  defp current_date?(%Date{} = value), do: value == Date.utc_today()

  defp current_date?(%NaiveDateTime{} = value), do: value |> NaiveDateTime.to_date() |> current_date?()

  defp current_date?(_), do: false

  defp truncate_datetime(%Date{} = value), do: value

  defp truncate_datetime(%NaiveDateTime{} = value) do
    {date, _time} = NaiveDateTime.to_erl(value)
    NaiveDateTime.from_erl!({date, {0, 0, 0}}) |> Cloak.Time.max_precision()
  end

  defp compatible_types?(:integer, :real), do: true
  defp compatible_types?(:real, :integer), do: true
  defp compatible_types?(type, type), do: true
  defp compatible_types?(_type1, _type2), do: false

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

  defp protect_against_join_timing_attacks(%Query{from: {:join, %{type: :inner_join}}} = query) do
    query = %Query{query | from: protect_joins(query.from, query, :invalid_row)}

    if branch_has_timing_protection?(query.from),
      # Since we are potentially generating additional subqueries here, we need to re-optimize the query
      # to make sure all filters are pushed downstream, so that the invalid row is not prematurely dropped.
      do: Cloak.Sql.Compiler.Optimizer.optimize(query),
      else: query
  end

  defp protect_against_join_timing_attacks(%Query{from: {:join, _}} = query),
    do: %Query{query | from: protect_joins(query.from, query, :not_exists)}

  defp protect_against_join_timing_attacks(query), do: query

  defp protect_joins({:join, join}, query, method) do
    {lhs, rhs} =
      if join.type == :right_outer_join do
        {protect_joins(join.lhs, query, method), protect_join_branch(join.rhs, query, method)}
      else
        {protect_join_branch(join.lhs, query, method), protect_joins(join.rhs, query, method)}
      end

    {:join, %{join | lhs: lhs, rhs: rhs}}
  end

  defp protect_joins(from, _query, _method), do: from

  defp query_has_non_key_filters?(query) do
    Lens.both(Lens.root(), simple_subquery_lens())
    |> Lens.filter(&(&1.type == :restricted))
    |> Lenses.filter_clauses()
    |> Lens.reject(&is_nil/1)
    |> Lens.reject(&key_comparison?/1)
    |> Lens.to_list(query) != []
  end

  defp simple_subquery_lens() do
    Lens.key(:from)
    |> Lens.filter(&match?({:subquery, _}, &1))
    |> Lens.at(1)
    |> Lens.reject(&(&1[:join_timing_protection] != nil))
    |> Lens.key(:ast)
    |> Lens.recur()
  end

  defp key_comparison?(%Expression{kind: :function, name: "=", args: [subject, target]}),
    do: Expression.key?(subject) and Expression.key?(target)

  defp key_comparison?(_), do: false

  defp query_needs_protection?(query), do: query_has_non_key_filters?(query)

  defp protect_join_branch({:subquery, subquery}, _query, method) do
    subquery =
      if query_needs_protection?(subquery.ast),
        do: Map.put(subquery, :join_timing_protection, method),
        else: subquery

    {:subquery, subquery}
  end

  defp protect_join_branch({:join, join}, query, :invalid_row) do
    lhs = protect_join_branch(join.lhs, query, :invalid_row)
    rhs = protect_join_branch(join.rhs, query, :invalid_row)

    {lhs, rhs} =
      if branch_has_timing_protection?(lhs) or branch_has_timing_protection?(rhs) do
        lhs = add_invalid_row_to_leafs(lhs, query)
        rhs = add_invalid_row_to_leafs(rhs, query)
        {lhs, rhs}
      else
        {lhs, rhs}
      end

    {:join, %{join | lhs: lhs, rhs: rhs}}
  end

  defp protect_join_branch({:join, join}, query, :not_exists) do
    lhs = protect_join_branch(join.lhs, query, :not_exists)
    rhs = protect_join_branch(join.rhs, query, :not_exists)
    {:join, %{join | lhs: lhs, rhs: rhs}}
  end

  defp protect_join_branch(table, _query, _method), do: table

  defp branch_has_timing_protection?({:join, join}),
    do: branch_has_timing_protection?(join.lhs) or branch_has_timing_protection?(join.rhs)

  defp branch_has_timing_protection?({:subquery, subquery}), do: subquery[:join_timing_protection] != nil
  defp branch_has_timing_protection?(_), do: false

  defp add_invalid_row_to_leafs({:join, join}, query) do
    lhs = add_invalid_row_to_leafs(join.lhs, query)
    rhs = add_invalid_row_to_leafs(join.rhs, query)
    {:join, %{join | lhs: lhs, rhs: rhs}}
  end

  defp add_invalid_row_to_leafs({:subquery, subquery}, _query),
    do: {:subquery, Map.put(subquery, :join_timing_protection, :invalid_row)}

  defp add_invalid_row_to_leafs(table_name, query) do
    table =
      query.table_aliases
      |> Enum.find(fn {alias, _table} -> alias == table_name end)
      |> case do
        {^table_name, table} -> table
        nil -> Enum.find(query.selected_tables, &(&1.name == table_name))
      end

    ast = %Query{
      command: :select,
      type: :standard,
      data_source: query.data_source,
      columns: Enum.map(table.columns, &Expression.column(&1, table)),
      column_titles: Enum.map(table.columns, & &1.name),
      from: table.name,
      selected_tables: [table]
    }

    {:subquery, %{ast: ast, join_timing_protection: :invalid_row, alias: table_name}}
  end
end
