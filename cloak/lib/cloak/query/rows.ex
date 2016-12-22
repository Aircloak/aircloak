defmodule Cloak.Query.Rows do
  @moduledoc "Functions for row processing, such as filtering and grouping."
  alias Cloak.Aql.{Expression, Function, Query}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a stream of rows passing all the given filters."
  @spec filter(Enumerable.t, [(any -> boolean)]) :: Enumerable.t
  def filter(rows, []), do: rows
  def filter(rows, filters), do:
    Stream.filter(rows, &Enum.all?(filters, fn(filter) -> filter.(&1) end))

  @doc "Selects and filters the rows according to query aggregators and properties."
  @spec extract_groups(Enumerable.t, Query.t) :: Enumerable.t
  def extract_groups(rows, query) do
    aggregated_columns =
      (query.property ++ query.aggregators)
      |> Enum.with_index()
      |> Enum.into(%{})
    rows
    |> Enum.filter(&filter_group(&1, aggregated_columns, query))
    |> Enum.map(&selected_values(&1, aggregated_columns, query))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp selected_values(row, aggregated_columns, query), do:
    for selected_column <- query.columns, do:
      fetch_value!(row, selected_column, aggregated_columns)

  defp fetch_value!(row, {column, :as, _}, columns), do: fetch_value!(row, column, columns)
  defp fetch_value!(row, {:function, _, args} = function, columns) do
    case Map.fetch(columns, function) do
      {:ok, index} -> Enum.at(row, index)
      :error -> Enum.map(args, &fetch_value!(row, &1, columns)) |> Function.apply(function)
    end
  end
  defp fetch_value!(_row, %Expression{constant?: true, value: value}, _columns), do: value
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
end
