defmodule Cloak.Sql.Compiler.Execution do
  @moduledoc """
  Makes the compiled query specification ready for execution.

  This is strictly speaking a post-compilation step where we do some transformation
  to the query, in order to be able to safely execute it. Here, we figure out
  things such as noise layers, alignments, or db column positions.
  """

  alias Cloak.DataSource
  alias Cloak.Sql.{CompilationError, Condition, Expression, FixAlign, Function, Query, Range}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.Sql.Query.Lenses
  alias Cloak.Query.DataEngine


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the query for execution."
  @spec prepare(Query.t) :: Query.t
  def prepare(%Query{command: :show} = query), do:
    query
  def prepare(%Query{command: :select} = query), do:
    query
    |> prepare_subqueries()
    |> censor_selected_uids()
    |> align_buckets()
    |> align_ranges(Lens.key(:where))
    |> align_join_ranges()
    |> optimize_columns_from_projected_tables()
    |> compile_sample_rate()
    |> Query.set_emulation_flag()
    |> partition_where_clauses()
    |> reject_null_user_ids()
    |> calculate_db_columns()
    |> compute_aggregators()

  @doc "Creates an executable query which describes a SELECT statement from a single table."
  @spec make_select_query(DataSource.t, DataSource.Table.t, [Expression.t]) :: Query.t
  def make_select_query(data_source, table, select_expressions) do
    column_titles = for expression <- select_expressions, do: expression.alias || expression.name
    calculate_db_columns(%Query{
      command: :select,
      subquery?: true,
      columns: select_expressions,
      column_titles: column_titles,
      from: table.name,
      data_source: data_source,
      selected_tables: [table]
    })
  end


  # -------------------------------------------------------------------
  # UID handling
  # -------------------------------------------------------------------

  defp reject_null_user_ids(%Query{subquery?: true} = query), do: query
  defp reject_null_user_ids(query), do:
    %{query | where: Condition.combine(:and, {:not, {:is, Helpers.id_column(query), :null}}, query.where)}

  defp censor_selected_uids(%Query{command: :select, subquery?: false} = query) do
    # In a top-level query, we're replacing all selected expressions which depend on uid columns with the `:*`
    # constant. This allows us to reduce the amount of anonymized values, without compromising the privacy.
    # For example, consider the query `select uid, name from users`. Normally, this would return only `(*, *)`
    # rows. However, with this replacement, we can return names which are frequent enough, without revealing
    # any sensitive information. For example, we could return: `(*, Alice), (*, Bob), (*, *)`, which is
    # not possible without this replacement.
    Lens.key(:columns)
    |> Lens.all()
    |> Lens.satisfy(&non_aggregated_uid_expression?/1)
    |> Lens.map(query, &Expression.constant(&1.type, :*))
  end
  defp censor_selected_uids(query), do: query

  # returns true if the column contains an expression with non-aggregated user ids
  defp non_aggregated_uid_expression?(column), do:
    ([column] |> get_in([Lenses.all_expressions()]) |> Enum.all?(&not &1.aggregate?)) and
    ([column] |> extract_columns() |> Enum.any?(& &1.user_id?))


  # -------------------------------------------------------------------
  # Bucket alignment
  # -------------------------------------------------------------------

  defp align_buckets(query) do
    {messages, query} = Lens.get_and_map(Lenses.buckets(), query, &align_bucket/1)
    Query.add_info(query, Enum.reject(messages, &is_nil/1))
  end

  defp align_bucket(column) do
    if Function.bucket_size(column) <= 0 do
      raise CompilationError, message: "Bucket size #{Function.bucket_size(column)} must be > 0"
    end

    aligned = Function.update_bucket_size(column, &FixAlign.align/1)
    if aligned == column do
      {nil, aligned}
    else
      {"Bucket size adjusted from #{Function.bucket_size(column)} to #{Function.bucket_size(aligned)}", aligned}
    end
  end

  defp optimize_columns_from_projected_tables(%Query{projected?: false} = query), do:
    # We're reducing the amount of selected columns from projected subqueries to only
    # those columns which we in fact need in the outer query (`query`).
    #
    # Notice that this has to be done after all verifications have been performed. The reason is that we're
    # conflating the list of selected columns and the list of available columns in the field `columns`.
    # Therefore, we need to perform all checks with all projected table columns selected, and only then can
    # we optimize the list of selected columns from the projected subquery.
    #
    # These two fields should likely be separated, and then we could invoke this function earlier. However,
    # even then, this function can only be invoked after `db_columns` have been calculated, because that is
    # the field we use to decide which columns from projected tables do we in fact need.
    Lens.map(Query.Lenses.direct_projected_subqueries(), query,
        &%{&1 | ast: optimized_projected_subquery_ast(&1.ast, required_column_names(query, &1.alias))})
  defp optimize_columns_from_projected_tables(%Query{projected?: true} = query), do:
    # If this query is projected, then the list was already optimized when the ast for this query
    # has been initially generated, so no need to do anything.
    query

  defp used_columns_from_table(query, table_name) do
    all_terminals = Lens.both(Lenses.terminals(), Lenses.join_conditions_terminals()) |> Lens.to_list(query)
    Lenses.leaf_expressions()
    |> Lens.to_list(all_terminals)
    |> Enum.filter(& &1.table != :unknown and &1.table.name == table_name)
    |> Enum.uniq_by(&Expression.id/1)
  end

  defp required_column_names(query, subquery_name), do:
    # all db columns of the outer query which are from this projected table, except the user id
    query |> used_columns_from_table(subquery_name) |> Enum.map(& &1.name)

  defp optimized_projected_subquery_ast(ast, required_column_names) do
    [user_id | columns] = ast.columns
    [user_id_title | column_titles] = ast.column_titles
    columns = [user_id | Enum.filter(columns, &(&1.alias || &1.name) in required_column_names)]
    titles = [user_id_title | Enum.filter(column_titles, & &1 in required_column_names)]
    %Query{ast | next_row_index: 0, db_columns: [], columns: columns, column_titles: titles}
    |> Query.set_emulation_flag()
    |> calculate_db_columns()
  end


  # -------------------------------------------------------------------
  # Subqueries
  # -------------------------------------------------------------------

  defp prepare_subqueries(query) do
    {info, prepared_subquery} = Lens.get_and_map(Query.Lenses.direct_subqueries(), query, fn(subquery) ->
      ast = prepare_subquery(subquery.ast)
      {ast.info, %{subquery | ast: ast}}
    end)

    Query.add_info(prepared_subquery, Enum.concat(info))
  end

  defp prepare_subquery(parsed_subquery), do:
    parsed_subquery
    |> prepare()
    |> align_limit()
    |> align_offset()
    |> align_ranges(Lens.key(:having))

  @minimum_subquery_limit 10
  defp align_limit(query = %{limit: nil}), do: query
  defp align_limit(query = %{limit: limit}) do
    aligned = limit |> FixAlign.align() |> round() |> max(@minimum_subquery_limit)
    if aligned != limit do
      %{query | limit: aligned}
      |> Query.add_info("Limit adjusted from #{limit} to #{aligned}")
    else
      query
    end
  end

  defp align_offset(query = %{offset: 0}), do: query
  defp align_offset(query = %{limit: limit, offset: offset}) do
    aligned = round(offset / limit) * limit
    if aligned != offset do
      %{query | offset: aligned}
      |> Query.add_info("Offset adjusted from #{offset} to #{aligned}")
    else
      query
    end
  end


  # -------------------------------------------------------------------
  # Normal validators and compilers
  # -------------------------------------------------------------------

  defp expand_arguments(column) do
    (column |> Expression.arguments() |> Enum.flat_map(&expand_arguments/1)) ++ [column]
  end

  defp compute_aggregators(%Query{group_by: [_|_]} = query), do:
    %Query{query | aggregators: Expression.unique_except(aggregators(query), &Expression.row_splitter?/1)}
  defp compute_aggregators(query) do
    case aggregators(query) do
      [] ->
        %Query{query | aggregators: [Expression.count_star()], implicit_count?: true}
      aggregators ->
        %Query{query | aggregators: Expression.unique_except(aggregators, &Expression.row_splitter?/1)}
    end
  end

  defp aggregators(query), do:
    (query.columns ++ having_columns(query) ++ order_by_columns(query.order_by))
    |> Enum.flat_map(&expand_arguments/1)
    |> Enum.filter(&(match?(%Expression{function?: true, aggregate?: true}, &1)))

  defp having_columns(query), do:
    Lenses.conditions()
    |> Lenses.operands()
    |> Lens.to_list(query.having)

  defp order_by_columns(order_by_clauses), do:
    Enum.map(order_by_clauses, fn({column, _direction}) -> column end)

  defp partition_where_clauses(query) do
    {emulated_where, where} = DataEngine.partitioned_where_clauses(query)
    %Query{query | where: where, emulated_where: emulated_where}
  end

  defp align_join_ranges(query), do:
    query
    |> Query.Lenses.join_condition_lenses()
    |> Enum.reduce(query, fn(lens, query) -> align_ranges(query, lens) end)

  defp align_ranges(query, lens) do
    clause = Lens.get(lens, query)
    grouped_inequalities = inequalities_by_column(clause)
    range_columns = Map.keys(grouped_inequalities)

    verify_ranges(grouped_inequalities)
    non_range_conditions = Condition.reject(clause, &Enum.member?(range_columns, Condition.subject(&1)))

    query = put_in(query, [lens], non_range_conditions)
    Enum.reduce(grouped_inequalities, query, &add_aligned_range(&1, &2, lens))
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
      range = {:and, lhs, rhs}
      update_in(query, [lens], &Condition.combine(:and, range, &1))
    else
      query
      |> add_clause(lens, {:comparison, column, :<, Expression.constant(column.type, right)})
      |> add_clause(lens, {:comparison, column, :>=, Expression.constant(column.type, left)})
      |> Query.add_info("The range for column #{Expression.display_name(column)} has been adjusted to #{left} <= "
        <> "#{Expression.short_name(column)} < #{right}.")
    end
    |> put_in([Lens.key(:ranges), Lens.front()], Range.new(column, {left, right}))
  end

  defp implement_range?({left, right}, conditions) do
    [{_, _, left_operator, left_column}, {_, _, right_operator, right_column}] =
      Enum.sort_by(conditions, &Condition.value/1, &Cloak.Data.lt_eq/2)

    left_operator == :>= && left_column.value == left && right_operator == :< && right_column.value == right
  end

  defp add_clause(query, lens, clause), do: Lens.map(lens, query, &Condition.combine(:and, clause, &1))

  defp verify_ranges(grouped_inequalities) do
    grouped_inequalities
    |> Enum.reject(fn({_, comparisons}) -> valid_range?(comparisons) end)
    |> case do
      [{column, _} | _] ->
        raise CompilationError, message:
          "Column #{Expression.display_name(column)} must be limited to a finite, nonempty range."
      _ -> :ok
    end
  end

  defp valid_range?(comparisons) do
    case Enum.sort_by(comparisons, &Condition.direction/1, &Kernel.>/2) do
      [cmp1, cmp2] ->
        Condition.direction(cmp1) != Condition.direction(cmp2) &&
          Cloak.Data.lt(Condition.value(cmp1), Condition.value(cmp2))
      _ -> false
    end
  end

  defp inequalities_by_column(where_clause) do
    Lenses.conditions()
    |> Lens.to_list(where_clause)
    |> Enum.filter(&Condition.inequality?/1)
    |> Enum.group_by(&Condition.subject/1)
    |> Enum.map(&discard_redundant_inequalities/1)
    |> Enum.into(%{})
  end

  defp discard_redundant_inequalities({column, inequalities}) do
    case {bottom, top} = Enum.partition(inequalities, &(Condition.direction(&1) == :>)) do
      {[], []} -> {column, []}
      {_, []} -> {column, [Enum.max_by(bottom, &Condition.value/1)]}
      {[], _} -> {column, [Enum.min_by(top, &Condition.value/1)]}
      {_, _} -> {column, [Enum.max_by(bottom, &Condition.value/1), Enum.min_by(top, &Condition.value/1)]}
    end
  end

  defp extract_columns(columns), do:
    Query.Lenses.leaf_expressions() |> Lens.to_list(columns)

  defp calculate_db_columns(query) do
    selected_columns = select_expressions(query)
    floated_columns = range_columns(query)
    {query, floated_columns} = Helpers.drop_redundant_floated_columns(query, selected_columns, floated_columns)
    selected_columns ++ floated_columns
    |> Enum.reduce(query, &Query.add_db_column(&2, &1))
  end

  defp range_columns(%{subquery?: true, emulated?: false}), do: []
  defp range_columns(%{ranges: ranges}), do: ranges |> Enum.map(&(&1.column)) |> extract_columns()

  defp select_expressions(%Query{command: :select, subquery?: true, emulated?: false} = query) do
    Enum.zip(query.column_titles, query.columns)
    |> Enum.map(fn({column_alias, column}) -> %Expression{column | alias: column_alias} end)
  end
  defp select_expressions(%Query{command: :select} = query) do
    # top-level query -> we're only fetching columns, while other expressions (e.g. function calls)
    # will be resolved in the post-processing phase
    used_columns = query
    |> needed_columns()
    |> extract_columns()
    |> Enum.reject(& &1.constant?)

    [Helpers.id_column(query) | used_columns]
  end

  defp needed_columns(query), do:
    [
      query.columns,
      query.group_by,
      query.emulated_where,
      query.having,
      Query.order_by_expressions(query),
      columns_required_for_splitting(query),
    ]

  defp columns_required_for_splitting(query), do:
    query
    |> Query.outermost_selected_splitters()
    |> get_in([Lenses.leaf_expressions()])
    |> Enum.reject(&(&1.constant? or &1.function?))

  defp compile_sample_rate(%Query{sample_rate: amount} = query) when amount != nil do
    true = is_integer(amount)
    # adds the condition for sampling: hash(user_id) % 100 < amount
    user_id_hash = Expression.function("hash", [Helpers.id_column(query)])
    user_id_ranged_hash = Expression.function("%", [user_id_hash, Expression.constant(:integer, 100)])
    sample_condition = {:comparison, user_id_ranged_hash, :<, Expression.constant(:integer, amount)}
    %Query{query | where: Condition.combine(:and, sample_condition, query.where)}
  end
  defp compile_sample_rate(query), do: query
end
