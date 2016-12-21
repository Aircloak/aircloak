defmodule Cloak.Query.DBEmulator.Selector do
  @moduledoc """
  Data retrieval for emulated queries.
  """

  alias Cloak.Aql.{Query, Comparison, Function, Expression}
  alias Cloak.Query.Sorter
  alias Cloak.Data


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes a `SELECT` query over the input stream of rows."
  @spec select(Enumerable.t, Query.t) :: Enumerable.t
  def select(stream, query) do
    stream
    |> filter_rows(query)
    |> select_columns(query)
    |> Sorter.order_rows(query)
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

  @doc "Applies the query conditions over the input stream of rows."
  @spec filter_rows(Enumerable.t, Query.t) :: Enumerable.t
  def filter_rows(stream, %Query{where: conditions}) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    apply_filters(stream, filters)
  end

  @doc "Keeps only the columns needed by the query from the selection target."
  @spec pick_db_columns(Enumerable.t, Query.t) :: Enumerable.t
  def pick_db_columns(stream, %Query{db_columns: db_columns, from: {:subquery, subquery}}) do
    # The column titles in a subquery are not guaranteed to be unique, but that is fine
    # since, if they are not, they can't be referenced exactly either.
    indices = for column <- db_columns, do:
      Enum.find_index(subquery.ast.column_titles, & &1 == column.name)
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
    stream
    |> Enum.reduce(%{}, fn(row, groups) ->
      property = Enum.map(query.property, &Function.apply_to_db_row(&1, row))
      values =
        groups
        |> Map.get(property, defaults)
        |> Enum.zip(accumulators)
        |> Enum.map(fn ({value, accumulator}) -> accumulator.(row, value) end)
      Map.put(groups, property, values)
    end)
    |> Stream.map(fn ({property, values}) ->
      values =
        Enum.zip(values, finalizers)
        |> Enum.map(fn ({value, finalizer}) -> finalizer.(value) end)
      property ++ values
    end)
    |> Cloak.Query.Aggregator.extract_groups(query)
  end
  defp select_columns(stream, %Query{columns: columns} = query) do
    Stream.map(stream, fn (row) ->
      Enum.map(columns, &Function.apply_to_db_row(&1, row))
    end)
    |> distinct(query)
  end

  defp offset_rows(stream, %Query{offset: 0}), do: stream
  defp offset_rows(stream, %Query{offset: offset}), do: Stream.drop(stream, offset)

  defp limit_rows(stream, %Query{limit: nil}), do: stream
  defp limit_rows(stream, %Query{limit: limit}), do: Stream.take(stream, limit)

  defp distinct(stream, %Query{distinct?: false}), do: stream
  defp distinct(stream, %Query{distinct?: true}), do: stream |> Enum.to_list() |> Enum.uniq()

  defp aggregator_to_default({:function, _name, [{:distinct, _column}]}), do: MapSet.new()
  defp aggregator_to_default({:function, "count", [_column]}), do: 0
  defp aggregator_to_default({:function, "min", [_column]}), do: nil
  defp aggregator_to_default({:function, "max", [_column]}), do: nil
  defp aggregator_to_default({:function, "sum", [_column]}), do: nil
  defp aggregator_to_default({:function, "avg", [_column]}), do: {nil, 0}
  defp aggregator_to_default({:function, "stddev", [_column]}), do: []
  defp aggregator_to_default({:function, "median", [_column]}), do: []

  defmacrop null_ignore_accumulator(do: expression) do
    quote do
      fn (row, var!(accumulator)) ->
        case Function.apply_to_db_row(var!(column), row) do
          nil -> var!(accumulator)
          var!(value) -> unquote expression
        end
      end
    end
  end

  defp aggregator_to_accumulator({:function, _name, [{:distinct, column}]}), do:
    null_ignore_accumulator do: MapSet.put(accumulator, value)
  defp aggregator_to_accumulator({:function, "count", [:*]}), do:
    fn (_row, count) -> count + 1 end
  defp aggregator_to_accumulator({:function, "count", [column]}) do
    null_ignore_accumulator do _ = value; accumulator + 1 end
  end
  defp aggregator_to_accumulator({:function, "sum", [column]}), do:
    null_ignore_accumulator do: (accumulator || 0) + value
  defp aggregator_to_accumulator({:function, "min", [column]}), do:
    null_ignore_accumulator do: Data.min(accumulator, value)
  defp aggregator_to_accumulator({:function, "max", [column]}), do:
    null_ignore_accumulator do: Data.max(accumulator, value)
  defp aggregator_to_accumulator({:function, "avg", [column]}) do
    null_ignore_accumulator do {sum, count} = accumulator; {(sum || 0) + value, count + 1} end
  end
  defp aggregator_to_accumulator({:function, "stddev", [column]}), do:
    null_ignore_accumulator do: [value | accumulator]
  defp aggregator_to_accumulator({:function, "median", [column]}), do:
    null_ignore_accumulator do: [value | accumulator]

  defp aggregator_to_finalizer({:function, "count", [{:distinct, _column}]}), do:
    &MapSet.size(&1)
  defp aggregator_to_finalizer({:function, "sum", [{:distinct, _column}]}), do:
    &if MapSet.size(&1) == 0, do: nil, else: Enum.sum(&1)
  defp aggregator_to_finalizer({:function, "min", [{:distinct, _column}]}), do:
    &if MapSet.size(&1) == 0, do: nil, else: Enum.min(&1)
  defp aggregator_to_finalizer({:function, "max", [{:distinct, _column}]}), do:
    &if MapSet.size(&1) == 0, do: nil, else: Enum.max(&1)
  defp aggregator_to_finalizer({:function, "avg", [{:distinct, _column}]}), do:
    &if MapSet.size(&1) == 0, do: nil, else: Enum.sum(&1) / MapSet.size(&1)
  defp aggregator_to_finalizer({:function, "stddev", [{:distinct, _column}]}), do:
    &if MapSet.size(&1) == 0, do: nil, else: stddev(&1)
  defp aggregator_to_finalizer({:function, "median", [{:distinct, _column}]}), do:
    &if MapSet.size(&1) == 0, do: nil, else: &1 |> MapSet.to_list() |> Enum.at(&1 |> MapSet.size() |> div(2))
  defp aggregator_to_finalizer({:function, "count", [_column]}), do: & &1
  defp aggregator_to_finalizer({:function, "sum", [_column]}), do: & &1
  defp aggregator_to_finalizer({:function, "min", [_column]}), do: & &1
  defp aggregator_to_finalizer({:function, "max", [_column]}), do: & &1
  defp aggregator_to_finalizer({:function, "avg", [_column]}), do:
    fn ({nil, 0}) -> nil; ({sum, count}) -> sum / count end
  defp aggregator_to_finalizer({:function, "stddev", [_column]}), do:
    fn ([]) -> nil; (values) -> stddev(values) end
  defp aggregator_to_finalizer({:function, "median", [_column]}), do:
    fn
      ([]) ->
        nil
      (values) ->
        values |> Enum.sort() |> Enum.at(values |> Enum.count() |> div(2))
    end

  defp stddev(values) do
    count = Enum.count(values)
    average = Enum.sum(values) / count
    variances = Enum.map(values, &(&1 - average) * (&1 - average))
    :math.sqrt(Enum.sum(variances) / count)
  end

  defp apply_filters(stream, []), do: stream
  defp apply_filters(stream, filters), do:
    Stream.filter(stream, &Enum.all?(filters, fn (filter) -> filter.(&1) end))

  defp joined_row_size({:subquery, subquery}), do: Enum.count(subquery.ast.db_columns)
  defp joined_row_size({:join, join}), do: joined_row_size(join.lhs) + joined_row_size(join.rhs)

  defp add_prefix_to_rows(stream, row), do: Stream.map(stream, &row ++ &1)

  defp add_suffix_to_rows(stream, row), do: Stream.map(stream, & &1 ++ row)

  defp cross_join(lhs, rhs), do: Stream.flat_map(lhs, &add_prefix_to_rows(rhs, &1))

  defp inner_join(lhs, rhs, join) do
    filters = Enum.map(join.conditions, &Comparison.to_function/1)
    Stream.flat_map(lhs, fn (lhs_row) ->
      rhs
      |> add_prefix_to_rows(lhs_row)
      |> apply_filters(filters)
    end)
  end

  defp left_join(lhs, rhs, join) do
    rhs_null_row = List.duplicate(nil, joined_row_size(join.rhs))
    outer_join(lhs, rhs, join.conditions, &add_prefix_to_rows/2, &[&1 ++ rhs_null_row], & &1)
  end

  defp right_join(lhs, rhs, join) do
    lhs_null_row = List.duplicate(nil, joined_row_size(join.lhs))
    outer_join(rhs, lhs, join.conditions, &add_suffix_to_rows/2, &[lhs_null_row ++ &1], & &1)
  end

  defp outer_join(lhs, rhs, conditions, rows_combiner, unmatched_handler, matched_handler) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    Stream.flat_map(lhs, fn (lhs_row) ->
      rhs
      |> rows_combiner.(lhs_row)
      |> apply_filters(filters)
      |> Enum.to_list()
      |> case do
        [] -> unmatched_handler.(lhs_row)
        joined_rows -> matched_handler.(joined_rows)
      end
    end)
  end

  defp full_join(lhs, rhs, join) do
    lhs_null_row = List.duplicate(nil, joined_row_size(join.lhs))
    unmatched_rhs = outer_join(rhs, lhs, join.conditions,
      &add_suffix_to_rows/2, &[lhs_null_row ++ &1], fn (_matches) -> [] end)
    lhs |> left_join(rhs, join) |> Stream.concat(unmatched_rhs)
  end

  defp get_column_index(columns, %Expression{function: "coalesce", function_args: args}), do:
    {:coalesce, Enum.map(args, &get_column_index(columns, &1))}
  defp get_column_index(columns, column), do:
    Enum.find_index(columns, &Expression.id(column) == Expression.id(&1))

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
end
