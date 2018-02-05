defmodule Cloak.Query.DbEmulator.Selector do
  @moduledoc """
  Data retrieval for emulated queries.
  """

  alias Cloak.Sql.{Query, Condition, Expression}
  alias Cloak.Query.{Rows, Sorter}
  alias Cloak.{Data, Stats}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes a `SELECT` query over the input stream of rows."
  @spec select(Enumerable.t, Query.t) :: Enumerable.t
  def select(stream, query) do
    columns = Query.bucket_columns(query)
    rows =
      stream
      |> Rows.filter(query |> Query.emulated_where() |> Condition.to_function())
      |> select_columns(columns, query)

    rows
    |> Sorter.order_rows(columns, query.order_by)
    |> offset_rows(query)
    |> limit_rows(query)
  end

  @doc "Joins two streams into one using the specified join type and conditions."
  @spec join(Enumerable.t, Enumerable.t, map) :: Enumerable.t
  def join(lhs, rhs, %{type: :cross_join}), do: cross_join(lhs, rhs)
  def join(lhs, rhs, %{type: :inner_join} = join), do: inner_join(lhs, rhs, join)
  def join(lhs, rhs, %{type: :left_outer_join} = join), do: left_join(lhs, rhs, join)
  def join(lhs, rhs, %{type: :right_outer_join} = join), do: right_join(lhs, rhs, join)

  @doc "Keeps only the columns needed by the query from the selection target."
  @spec pick_db_columns(Enumerable.t, Query.t) :: Enumerable.t
  def pick_db_columns(stream, query = %Query{from: {:subquery, _}}), do: do_pick_db_columns(stream, query)
  def pick_db_columns(stream, query = %Query{from: {:join, _}}), do: do_pick_db_columns(stream, query)
  def pick_db_columns(stream, _query), do: stream


  # -------------------------------------------------------------------
  # Implementation of pick_db_columns
  # -------------------------------------------------------------------

  defp do_pick_db_columns(stream, %Query{db_columns: db_columns, from: from}) do
    # The column titles in a subquery are not guaranteed to be unique, but that is fine
    # since, if they are not, they can't be referenced exactly either.
    columns =
      db_columns
      |> Enum.sort_by(& &1.row_index)
      |> Enum.map(&update_row_index(&1, from))

    Stream.map(stream, fn (row) ->
      Enum.map(columns, &Expression.value(&1, row))
    end)
  end

  defp update_row_index(column = %{function?: true, function_args: function_args}, source), do:
    %Expression{column| function_args: Enum.map(function_args, &update_row_index(&1, source))}
  defp update_row_index(column, from), do:
    %Expression{column | row_index: index_in_from(column, from)}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp select_columns(stream, columns, %Query{group_by: [_|_]} = query) do
    defaults = Enum.map(query.aggregators, &aggregator_to_default/1)
    accumulators = Enum.map(query.aggregators, &aggregator_to_accumulator/1)
    finalizers = Enum.map(query.aggregators, &aggregator_to_finalizer/1)

    stream
    |> Rows.group(query, defaults,
      fn(aggregated_values, row) ->
        aggregated_values
        |> Enum.zip(accumulators)
        |> Enum.map(fn ({value, accumulator}) -> accumulator.(row, value) end)
      end
    )
    |> Stream.map(fn ({group_values, aggregated_values}) ->
      aggregated_values =
        aggregated_values
        |> Enum.zip(finalizers)
        |> Enum.map(fn({value, finalizer}) -> finalizer.(value) end)
      group_values ++ aggregated_values
    end)
    |> Rows.extract_groups(columns, query)
  end
  defp select_columns(stream, columns, _query) do
    Stream.map(stream, fn(row) -> Enum.map(columns, &Expression.value(&1, row)) end)
  end

  defp offset_rows(stream, %Query{offset: 0}), do: stream
  defp offset_rows(stream, %Query{offset: offset}), do: Stream.drop(stream, offset)

  defp limit_rows(stream, %Query{limit: nil}), do: stream
  defp limit_rows(stream, %Query{limit: limit}), do: Stream.take(stream, limit)

  defp aggregator_to_default(%Expression{function?: true, function_args: [{:distinct, _column}]}), do: MapSet.new()
  defp aggregator_to_default(%Expression{function?: true, function: "count", function_args: [_column]}), do: 0
  defp aggregator_to_default(%Expression{function?: true, function: "min", function_args: [_column]}), do: nil
  defp aggregator_to_default(%Expression{function?: true, function: "max", function_args: [_column]}), do: nil
  defp aggregator_to_default(%Expression{function?: true, function: "sum", function_args: [_column]}), do: nil
  defp aggregator_to_default(%Expression{function?: true, function: "avg", function_args: [_column]}), do: {nil, 0}
  defp aggregator_to_default(%Expression{function?: true, function: "stddev", function_args: [_column]}), do: []
  defp aggregator_to_default(%Expression{function?: true, function: "median", function_args: [_column]}), do: []

  defmacrop null_ignore_accumulator(do: expression) do
    quote do
      fn (row, var!(accumulator)) ->
        case Expression.value(var!(column), row) do
          nil -> var!(accumulator)
          var!(value) -> unquote expression
        end
      end
    end
  end

  defp aggregator_to_accumulator(%Expression{function?: true, function_args: [{:distinct, column}]}), do:
    null_ignore_accumulator do: MapSet.put(accumulator, value)
  defp aggregator_to_accumulator(%Expression{function?: true, function: "count", function_args: [:*]}), do:
    fn (_row, count) -> count + 1 end
  defp aggregator_to_accumulator(%Expression{function?: true, function: "count", function_args: [column]}) do
    null_ignore_accumulator do _ = value; accumulator + 1 end
  end
  defp aggregator_to_accumulator(%Expression{function?: true, function: "sum", function_args: [column]}), do:
    null_ignore_accumulator do: (accumulator || 0) + value
  defp aggregator_to_accumulator(%Expression{function?: true, function: "min", function_args: [column]}), do:
    null_ignore_accumulator do: Data.min(accumulator, value)
  defp aggregator_to_accumulator(%Expression{function?: true, function: "max", function_args: [column]}), do:
    null_ignore_accumulator do: Data.max(accumulator, value)
  defp aggregator_to_accumulator(%Expression{function?: true, function: "avg", function_args: [column]}) do
    null_ignore_accumulator do {sum, count} = accumulator; {(sum || 0) + value, count + 1} end
  end
  defp aggregator_to_accumulator(%Expression{function?: true, function: "stddev", function_args: [column]}), do:
    null_ignore_accumulator do: [value | accumulator]
  defp aggregator_to_accumulator(%Expression{function?: true, function: "median", function_args: [column]}), do:
    null_ignore_accumulator do: [value | accumulator]

  defp aggregator_to_finalizer(%Expression{function?: true, function: "count", function_args: [{:distinct, _column}]}),
    do: &MapSet.size(&1)
  defp aggregator_to_finalizer(%Expression{function?: true, function: "sum", function_args: [{:distinct, _column}]}),
    do: & &1 |> MapSet.to_list() |> Stats.sum()
  defp aggregator_to_finalizer(%Expression{function?: true, function: "min", function_args: [{:distinct, _column}]}),
    do: &if MapSet.size(&1) == 0, do: nil, else: set_min(&1)
  defp aggregator_to_finalizer(%Expression{function?: true, function: "max", function_args: [{:distinct, _column}]}),
    do: &if MapSet.size(&1) == 0, do: nil, else: set_max(&1)
  defp aggregator_to_finalizer(%Expression{function?: true, function: "avg", function_args: [{:distinct, _column}]}),
    do: & &1 |> MapSet.to_list() |> Stats.mean()
  defp aggregator_to_finalizer(%Expression{function?: true, function: "stddev", function_args: [{:distinct, _column}]}),
    do: & &1 |> MapSet.to_list() |> Stats.stddev()
  defp aggregator_to_finalizer(%Expression{function?: true, function: "median",
    function_args: [{:distinct, %Expression{type: :number}}]}), do: & &1 |> MapSet.to_list() |> Stats.median()
  defp aggregator_to_finalizer(%Expression{function?: true, function: "median", function_args: [{:distinct, _column}]}),
    do: &if MapSet.size(&1) == 0, do: nil, else: &1 |> sort() |> Enum.at(&1 |> MapSet.size() |> div(2))
  defp aggregator_to_finalizer(%Expression{function?: true, function: "count", function_args: [_column]}), do: & &1
  defp aggregator_to_finalizer(%Expression{function?: true, function: "sum", function_args: [_column]}), do: & &1
  defp aggregator_to_finalizer(%Expression{function?: true, function: "min", function_args: [_column]}), do: & &1
  defp aggregator_to_finalizer(%Expression{function?: true, function: "max", function_args: [_column]}), do: & &1
  defp aggregator_to_finalizer(%Expression{function?: true, function: "avg", function_args: [_column]}), do:
    fn ({nil, 0}) -> nil; ({sum, count}) -> sum / count end
  defp aggregator_to_finalizer(%Expression{function?: true, function: "stddev", function_args: [_column]}), do:
    &Stats.stddev/1
  defp aggregator_to_finalizer(%Expression{function?: true, function: "median",
    function_args: [%Expression{type: :number}]}), do: &Stats.median/1
  defp aggregator_to_finalizer(%Expression{function?: true, function: "median", function_args: [_column]}), do:
    &if &1 == [], do: nil, else: &1 |> sort() |> Enum.at(&1 |> Enum.count() |> div(2))

  defp set_min(set), do: Enum.reduce(set, &Data.min/2)

  defp set_max(set), do: Enum.reduce(set, &Data.max/2)

  defp sort(values), do: Enum.sort(values, &Data.lt_eq/2)

  defp joined_row_size({:subquery, subquery}), do: Enum.count(subquery.ast.columns)
  defp joined_row_size({:join, join}), do: joined_row_size(join.lhs) + joined_row_size(join.rhs)

  defp add_prefix_to_rows(stream, row), do: Stream.map(stream, &row ++ &1)

  defp add_suffix_to_rows(stream, row), do: Stream.map(stream, & &1 ++ row)

  defp cross_join(lhs, rhs), do: Stream.flat_map(lhs, &add_prefix_to_rows(rhs, &1))

  defp inner_join(lhs, rhs, join) do
    rhs_pre_filter = create_join_pre_filter(rhs, join)
    filter = Condition.to_function(join.conditions)
    Stream.flat_map(lhs, fn (lhs_row) ->
      lhs_row
      |> rhs_pre_filter.()
      |> add_prefix_to_rows(lhs_row)
      |> Rows.filter(filter)
    end)
  end

  defp left_join(lhs, rhs, join) do
    rhs_null_row = List.duplicate(nil, joined_row_size(join.rhs))
    outer_join(lhs, rhs, join, &add_prefix_to_rows/2, &[&1 ++ rhs_null_row], & &1)
  end

  defp right_join(lhs, rhs, join) do
    lhs_null_row = List.duplicate(nil, joined_row_size(join.lhs))
    outer_join(rhs, lhs, join, &add_suffix_to_rows/2, &[lhs_null_row ++ &1], & &1)
  end

  defp outer_join(lhs, rhs, join, rows_combiner, unmatched_handler, matched_handler) do
    rhs_pre_filter = create_join_pre_filter(rhs, join)
    filter = Condition.to_function(join.conditions)
    Stream.flat_map(lhs, fn (lhs_row) ->
      lhs_row
      |> rhs_pre_filter.()
      |> rows_combiner.(lhs_row)
      |> Rows.filter(filter)
      |> Enum.to_list()
      |> case do
        [] -> unmatched_handler.(lhs_row)
        joined_rows -> matched_handler.(joined_rows)
      end
    end)
  end

  # This function returns a functor that pre-filters right side rows in order to drastically improve join performance.
  # It does that by grouping rows by one of the matching columns in the join conditions.
  # For now, we assume at least an equality condition for the join always exists.
  defp create_join_pre_filter(rhs_rows, join) do
    {lhs, rhs} = extract_matching_columns_from_join(join)
    lhs_match_index = index_in_from(lhs, join.lhs)
    rhs_match_index = index_in_from(rhs, join.rhs)

    rhs_rows_map = Enum.group_by(rhs_rows, &Enum.at(&1, rhs_match_index))
    fn (lhs_row) ->
      lhs_match_value = Enum.at(lhs_row, lhs_match_index)
      Map.get(rhs_rows_map, lhs_match_value, [])
    end
  end

  defp extract_matching_columns_from_join(join) do
    {subject, target} = best_condition_for_matching(join)

    # Make sure we return the columns in the correct order ({left_branch, right_branch}).
    if table_is_in_join_branch?(subject.table.name, join.lhs) do
      true = table_is_in_join_branch?(target.table.name, join.rhs)
      {subject, target}
    else
      true = table_is_in_join_branch?(target.table.name, join.lhs)
      true = table_is_in_join_branch?(subject.table.name, join.rhs)
      {target, subject}
    end
  end

  defp best_condition_for_matching(join) do
    conditions = Lens.to_list(Query.Lenses.conditions(), join.conditions)

    for {:comparison, subject, :=, target} <- conditions, subject != target do
      {subject, target}
    end
    |> Enum.max_by(&prefer_user_id/1, fn() -> raise "At least one condition should exist." end)
  end

  defp prefer_user_id({subject, target}), do:
    user_id_score(subject) + user_id_score(target)

  defp user_id_score(%Expression{user_id?: true}), do: 1
  defp user_id_score(_), do: 0

  defp table_is_in_join_branch?(table_name, {:join, join}), do:
    table_is_in_join_branch?(table_name, join.lhs) or table_is_in_join_branch?(table_name, join.rhs)
  defp table_is_in_join_branch?(table_name, {:subquery, %{alias: subquery_alias}}), do: table_name == subquery_alias
  defp table_is_in_join_branch?(table_name, joined_table), do: table_name == joined_table

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)


  # -------------------------------------------------------------------
  # Indexing helpers
  # -------------------------------------------------------------------

  defp index_in_from(column, {:subquery, %{ast: source_subquery}}), do:
    source_subquery.column_titles
    |> Enum.find_index(&insensitive_equal?(&1, column.name))
    |> check_index(column, source_subquery.column_titles)
  defp index_in_from(column, {:join, join}), do:
    join.columns |> join_column_index(column) |> check_index(column, join.columns)

  defp join_column_index(columns, column), do:
    Enum.find_index(columns, &Expression.id(column) == Expression.id(&1)) ||
      Enum.find_index(columns, &insensitive_equal?(Expression.id(column), &1.name))

  defp check_index(nil, column, targets), do:
    raise "Column index for column #{inspect(column, pretty: true)} could not be found in the " <>
      "list of available options: #{inspect(targets, pretty: true)}"
  defp check_index(index, _column, _targets), do: index
end
