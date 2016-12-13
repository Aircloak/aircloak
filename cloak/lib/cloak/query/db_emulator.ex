defmodule Cloak.Query.DBEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """

  alias Cloak.Aql.{Query, Comparison, Function, Column}
  alias Cloak.Query.Sorter


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
  def join(lhs, rhs, %{type: :cross_join}) do
    Stream.flat_map(lhs, fn (lhs_row) ->
      rhs
      |> Stream.map(fn (rhs_row) -> lhs_row ++ rhs_row end)
    end)
  end
  def join(lhs, rhs, %{conditions: conditions, type: :inner_join}) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    Stream.flat_map(lhs, fn (lhs_row) ->
      rhs
      |> Stream.map(fn (rhs_row) -> lhs_row ++ rhs_row end)
      |> apply_filters(filters)
    end)
  end
  def join(lhs, rhs, %{type: :left_outer_join} = join) do
    filters = Enum.map(join.conditions, &Comparison.to_function/1)
    rhs_null_row = List.duplicate(nil, joined_row_size(join.rhs))
    Stream.flat_map(lhs, fn (lhs_row) ->
      rhs
      |> Stream.map(fn (rhs_row) -> lhs_row ++ rhs_row end)
      |> apply_filters(filters)
      |> Enum.to_list()
      |> case do
        [] -> [lhs_row ++ rhs_null_row]
        joined_rows -> joined_rows
      end
    end)
  end
  def join(lhs, rhs, %{type: :right_outer_join} = join) do
    filters = Enum.map(join.conditions, &Comparison.to_function/1)
    lhs_null_row = List.duplicate(nil, joined_row_size(join.lhs))
    Stream.flat_map(rhs, fn (rhs_row) ->
      lhs
      |> Stream.map(fn (lhs_row) -> lhs_row ++ rhs_row end)
      |> apply_filters(filters)
      |> Enum.to_list()
      |> case do
        [] -> [lhs_null_row ++ rhs_row]
        joined_rows -> joined_rows
      end
    end)
  end
  def join(lhs, rhs, %{type: :full_outer_join} = join) do
    filters = Enum.map(join.conditions, &Comparison.to_function/1)
    lhs_null_row = List.duplicate(nil, joined_row_size(join.lhs))
    rhs_null_row = List.duplicate(nil, joined_row_size(join.rhs))
    left_stream =
      Stream.flat_map(lhs, fn (lhs_row) ->
        rhs
        |> Stream.map(fn (rhs_row) -> lhs_row ++ rhs_row end)
        |> apply_filters(filters)
        |> Enum.to_list()
        |> case do
          [] -> [lhs_row ++ rhs_null_row]
          joined_rows -> joined_rows
        end
      end)
    right_stream =
      Stream.flat_map(rhs, fn (rhs_row) ->
        lhs
        |> Stream.map(fn (lhs_row) -> lhs_row ++ rhs_row end)
        |> apply_filters(filters)
        |> Enum.to_list()
        |> case do
          [] -> [lhs_null_row ++ rhs_row]
          _joined_rows -> []
        end
      end)
    Stream.concat(left_stream, right_stream)
  end

  @doc "Applies the query conditions over the input stream of rows."
  @spec filter_rows(Enumerable.t, Query.t) :: Enumerable.t
  def filter_rows(stream, %Query{where: conditions}) do
    filters = Enum.map(conditions, &Comparison.to_function/1)
    apply_filters(stream, filters)
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
        |> Enum.map(fn ({value, accumulator}) -> accumulator.(row, value) end)
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
    null_ignore_accumulator do: min(accumulator, value)
  defp aggregator_to_accumulator({:function, "max", [column]}), do:
    null_ignore_accumulator do: max(accumulator, value) || value
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
end
