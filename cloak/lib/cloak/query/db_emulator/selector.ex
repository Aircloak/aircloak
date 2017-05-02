defmodule Cloak.Query.DbEmulator.Selector do
  @moduledoc """
  Data retrieval for emulated queries.
  """

  alias Cloak.Sql.{Query, Comparison, Expression}
  alias Cloak.Query.{Rows, Sorter}
  alias Cloak.{Data, Stats}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes a `SELECT` query over the input stream of rows."
  @spec select(Enumerable.t, Query.t) :: Enumerable.t
  def select(stream, query) do
    {columns, rows} =
      stream
      |> Rows.filter(Enum.map(query.where, &Comparison.to_function/1))
      |> select_columns(query)

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
  def join(lhs, rhs, %{type: :full_outer_join} = join), do: full_join(lhs, rhs, join)

  @doc "Keeps only the columns needed by the query from the selection target."
  @spec pick_db_columns(Enumerable.t, Query.t) :: Enumerable.t
  def pick_db_columns(stream, %Query{db_columns: db_columns, from: {:subquery, subquery}}) do
    # The column titles in a subquery are not guaranteed to be unique, but that is fine
    # since, if they are not, they can't be referenced exactly either.
    indices = for column <- db_columns, do:
      Enum.find_index(subquery.ast.column_titles, &insensitive_equal?(&1, column.name))
    pick_columns(stream, indices)
  end
  def pick_db_columns(stream, %Query{db_columns: db_columns, from: {:join, join}}) do
    indices = for column <- db_columns, do:
      get_column_index(join.columns, column)
    pick_columns(stream, indices)
  end
  def pick_db_columns(stream, _query), do: stream


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp select_columns(stream, %Query{group_by: [_|_]} = query) do
    defaults = Enum.map(query.aggregators, &aggregator_to_default/1)
    accumulators = Enum.map(query.aggregators, &aggregator_to_accumulator/1)
    finalizers = Enum.map(query.aggregators, &aggregator_to_finalizer/1)

    columns = Query.bucket_columns(query)
    rows =
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

    {columns, rows}
  end
  defp select_columns(stream, query) do
    columns = Rows.group_expressions(query)
    {columns,
      stream
      |> Stream.map(fn(row) -> Enum.map(columns, &Expression.value(&1, row)) end)
      |> distinct(query)
    }
  end

  defp offset_rows(stream, %Query{offset: 0}), do: stream
  defp offset_rows(stream, %Query{offset: offset}), do: Stream.drop(stream, offset)

  defp limit_rows(stream, %Query{limit: nil}), do: stream
  defp limit_rows(stream, %Query{limit: limit}), do: Stream.take(stream, limit)

  defp distinct(stream, %Query{distinct?: false}), do: stream
  defp distinct(stream, %Query{distinct?: true}), do: stream |> Enum.to_list() |> Enum.uniq()

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
    filters = Enum.map(join.conditions, &Comparison.to_function/1)
    Stream.flat_map(lhs, fn (lhs_row) ->
      lhs_row
      |> rhs_pre_filter.()
      |> add_prefix_to_rows(lhs_row)
      |> Rows.filter(filters)
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
    filters = Enum.map(join.conditions, &Comparison.to_function/1)
    Stream.flat_map(lhs, fn (lhs_row) ->
      lhs_row
      |> rhs_pre_filter.()
      |> rows_combiner.(lhs_row)
      |> Rows.filter(filters)
      |> Enum.to_list()
      |> case do
        [] -> unmatched_handler.(lhs_row)
        joined_rows -> matched_handler.(joined_rows)
      end
    end)
  end

  defp full_join(lhs, rhs, join) do
    lhs_null_row = List.duplicate(nil, joined_row_size(join.lhs))
    unmatched_rhs = outer_join(rhs, lhs, join, &add_suffix_to_rows/2, &[lhs_null_row ++ &1], fn (_matches) -> [] end)
    lhs |> left_join(rhs, join) |> Stream.concat(unmatched_rhs)
  end

  defp get_column_index(columns, %Expression{function: "coalesce", function_args: args}), do:
    {:coalesce, Enum.map(args, &get_column_index(columns, &1))}
  defp get_column_index(columns, column), do:
    Enum.find_index(columns, &Expression.id(column) == Expression.id(&1)) ||
      Enum.find_index(columns, &insensitive_equal?(Expression.id(column), &1.name))

  defp pick_value(_row, {:coalesce, []}), do: nil
  defp pick_value(row, {:coalesce, [index | rest]}) do
    case pick_value(row, index)  do
      nil -> pick_value(row, {:coalesce, rest})
      value -> value
    end
  end
  defp pick_value(row, index) when is_integer(index), do: Enum.at(row, index)

  defp pick_columns(stream, indices) do
    Stream.map(stream, fn (row) ->
      Enum.map(indices, &pick_value(row, &1))
    end)
  end

  # This function returns a functor that pre-filters right side rows in order to drastically improve join performance.
  # It does that by grouping rows by one of the matching columns in the join conditions.
  # For now, we assume at least an equality condition for the join always exists.
  defp create_join_pre_filter(rhs_rows, join) do
    {%Expression{row_index: lhs_match_index}, %Expression{row_index: rhs_match_index}} =
      extract_matching_columns_from_join(join)
    rhs_match_index = rhs_match_index - joined_row_size(join.lhs)
    rhs_rows_map = Enum.group_by(rhs_rows, &Enum.at(&1, rhs_match_index))
    fn (lhs_row) ->
      lhs_match_value = Enum.at(lhs_row, lhs_match_index)
      Map.get(rhs_rows_map, lhs_match_value, [])
    end
  end

  defp column_evaluator(%Expression{user_id?: true}), do: 1
  defp column_evaluator(_), do: 0

  defp condition_evaluator({subject, target}), do:
    column_evaluator(subject) + column_evaluator(target)

  defp extract_matching_columns_from_join(join) do
    # Get best equality comparison between left and right columns (preferring user id columns).
    [{subject, target} | _] =
      (for {:comparison, subject, :=, target} <- join.conditions, subject != target, do: {subject, target})
      |> Enum.sort_by(&condition_evaluator/1, &>=/2)
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

  defp table_is_in_join_branch?(table_name, {:join, join}), do:
    table_is_in_join_branch?(table_name, join.lhs) or table_is_in_join_branch?(table_name, join.rhs)
  defp table_is_in_join_branch?(table_name, {:subquery, %{alias: subquery_alias}}), do: table_name == subquery_alias
  defp table_is_in_join_branch?(table_name, joined_table), do: table_name == joined_table

  defp insensitive_equal?(s1, s2), do: String.downcase(s1) == String.downcase(s2)
end
