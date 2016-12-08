defmodule Cloak.Query.DBEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """

  alias Cloak.Aql.{Query, Comparison, Function, Column}
  alias Cloak.Query.Sorter
  alias Cloak.Query.Runner.RuntimeError


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

  @doc "Joins two streams into one using the "
  @spec join(Enumerable.t, Enumerable.t, Cloak.Aql.Parser.join) :: Enumerable.t
  def join(_lhs, _rhs, _join), do: raise RuntimeError, message: "Emulation of JOINs is not yet supported."

  @doc "Applies the query conditions over the input stream of rows."
  @spec filter_rows(Enumerable.t, Query.t) :: Enumerable.t
  def filter_rows(stream, %Query{where: []}), do: stream
  def filter_rows(stream, %Query{where: conditions}) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    Stream.filter(stream, &Enum.all?(filters, fn (filter) -> filter.(&1) end))
  end

  @doc "Selects and filters the rows previously grouped by an aggregator."
  @spec extract_groups(Enumerable.t, Query.t) :: Enumerable.t
  def extract_groups(stream, query) do
    aggregated_columns =
      (query.property ++ query.aggregators)
      |> Enum.with_index()
      |> Enum.into(%{})
    stream
    |> Enum.filter(&filter_group(&1, aggregated_columns, query))
    |> Enum.map(&selected_values(&1, aggregated_columns, query))
  end


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
        |> Enum.map(fn ({value, accumulator}) -> accumulator.(value, row) end)
      Map.put(groups, property, values)
    end)
    |> Stream.map(fn ({property, values}) ->
      values =
        Enum.zip(values, finalizers)
        |> Enum.map(fn ({value, finalizer}) -> finalizer.(value) end)
      property ++ values
    end)
    |> extract_groups(query)
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

  defp selected_values(row, aggregated_columns, query), do:
    for selected_column <- query.columns, do:
      fetch_value!(row, selected_column, aggregated_columns)

  defp fetch_value!(row,  {:function, _, args} = function, columns) do
    case Map.fetch(columns, function) do
      {:ok, index} -> Enum.at(row, index)
      :error -> Enum.map(args, &fetch_value!(row, &1, columns)) |> Function.apply(function)
    end
  end
  defp fetch_value!(_row, %Column{constant?: true, value: value}, _columns), do: value
  defp fetch_value!(row, column, columns), do: Enum.at(row, Map.fetch!(columns, column))

  defp filter_group(row, columns, query), do:
    Enum.all?(query.having, &matches_having_condition?(row, &1, columns))

  defp matches_having_condition?(row, {:comparison, column, operator, target}, columns) do
    value = fetch_value!(row, column, columns)
    target = fetch_value!(row, target, columns)
    compare(value, operator, target)
  end

  defp compare(nil, _op, _target), do: false
  defp compare(_value, _op, nil), do: false
  defp compare(value, :=, target), do: value == target
  defp compare(value, :<, target), do: value < target
  defp compare(value, :<=, target), do: value <= target
  defp compare(value, :>, target), do: value > target
  defp compare(value, :>=, target), do: value >= target
  defp compare(value, :<>, target), do: value != target

  defp aggregator_to_default({:function, _name, [{:distinct, _column}]}), do: MapSet.new()
  defp aggregator_to_default({:function, "count", [_column]}), do: 0
  defp aggregator_to_default({:function, "min", [_column]}), do: nil
  defp aggregator_to_default({:function, "max", [_column]}), do: nil
  defp aggregator_to_default({:function, "sum", [_column]}), do: nil
  defp aggregator_to_default({:function, "avg", [_column]}), do: {nil, 0}
  defp aggregator_to_default({:function, "stddev", [_column]}), do: []
  defp aggregator_to_default({:function, "median", [_column]}), do: []

  defp aggregator_to_accumulator({:function, _name, [{:distinct, column}]}), do:
    fn (accumulator, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> accumulator
        value -> MapSet.put(accumulator, value)
      end
    end
  defp aggregator_to_accumulator({:function, "count", [:*]}), do:
    fn (count, _row) -> count + 1 end
  defp aggregator_to_accumulator({:function, "count", [column]}), do:
    fn (accumulator, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> accumulator
        _value -> accumulator + 1
      end
    end
  defp aggregator_to_accumulator({:function, "sum", [column]}), do:
    fn (accumulator, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> accumulator
        value -> (accumulator || 0) + value
      end
    end
  defp aggregator_to_accumulator({:function, "min", [column]}), do:
    fn (accumulator, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> accumulator
        value -> min(accumulator, value)
      end
    end
  defp aggregator_to_accumulator({:function, "max", [column]}), do:
    fn (accumulator, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> accumulator
        value -> max(accumulator, value) || value
      end
    end
  defp aggregator_to_accumulator({:function, "avg", [column]}), do:
    fn ({sum, count}, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> {sum, count}
        value -> {(sum || 0) + value, count + 1}
      end
    end
  defp aggregator_to_accumulator({:function, "stddev", [column]}), do:
    fn (accumulator, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> accumulator
        value -> [value | accumulator]
      end
    end
  defp aggregator_to_accumulator({:function, "median", [column]}), do:
    fn (accumulator, row) ->
      case Function.apply_to_db_row(column, row) do
        nil -> accumulator
        value -> [value | accumulator]
      end
    end

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
    values = Enum.map(values, &(&1 - average) * (&1 - average))
    Enum.sum(values) / count
  end
end
