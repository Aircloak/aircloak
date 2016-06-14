defmodule Cloak.Query.Result do
  @moduledoc "Contains functions for converting buckets to a columns/rows representation."

  import Cloak.Type
  alias Cloak.SqlQuery


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Converts a list of buckets into aggregate rows for reporting.
  The consuming client will still have to expand the rows to mimic normal SQL
  where individual rows are produced.
  """

  @spec map_buckets([Bucket.t], SqlQuery.t) :: [Row.t]
  def map_buckets(buckets, %{implicit_count: true} = query) do
    for {property, [count]} <- buckets, do: %{row: bucket_to_row(query, property, []), occurrences: count}
  end
  def map_buckets(buckets, query) do
    for {property, aggregated_values} <- buckets,
      do: %{row: bucket_to_row(query, property, aggregated_values), occurrences: 1}
  end

  @spec group_by_property([Property.t], [String.t], SqlQuery.t) :: [GroupedRow.t]
  def group_by_property(rows, columns, query) do
    columns_start = List.duplicate([], length(query.aggregators))
    rows
    |> Enum.reduce(%{}, fn([user | fields], accumulator) ->
        property = for column <- query.property, do: extract_field(fields, columns, column)
        {old_users, old_columns} = Map.get(accumulator, property, {[], columns_start})
        new_users = [user | old_users]
        values = for {:function, _, column} <- query.aggregators, do: extract_field(fields, columns, column)
        new_columns = for {new_value, old_column} <- Enum.zip(values, old_columns), do: [new_value | old_column]
        Map.put(accumulator, property, {new_users, new_columns})
      end)
    |> Enum.to_list()
  end

  @doc "Sorts the rows in the order defined in the query."
  @spec order_rows([Row.t], SqlQuery.t) :: [Row.t]
  def order_rows(rows, %{order_by: order_list}) do
    Enum.sort(rows, fn(%{row: row1}, %{row: row2}) ->
      compare_rows(row1, row2, order_list)
    end)
  end
  def order_rows(rows, _), do: rows


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp bucket_to_row(query, property, aggregated_values) do
    for column <- query.columns do
      case column do
        {:function, _, _} ->
          extract_field(aggregated_values, query.aggregators, column)
        identifier ->
          extract_field(property, query.property, identifier)
      end
    end
  end

  defp extract_field(_fields, _columns, :*) do
    :*
  end
  defp extract_field(fields, columns, column) do
    index = Enum.find_index(columns, &(&1 === column))
    Enum.at(fields, index)
  end

  defp compare_rows(row1, row2, []), do: row1 < row2
  defp compare_rows(row1, row2, [{index, direction} | remaining_order]) do
    field1 = row1 |> Enum.at(index)
    field2 = row2 |> Enum.at(index)
    case field1 === field2 do
      :true -> compare_rows(row1, row2, remaining_order)
      :false -> compare_fields(field1, field2, direction)
    end
  end

  defp compare_fields(field1, field2, nil), do: compare_fields(field1, field2, :asc)
  defp compare_fields(:*, _, _), do: false
  defp compare_fields(_, :*, _), do: true
  defp compare_fields(nil, _, :asc), do: false
  defp compare_fields(_, nil, :asc), do: true
  defp compare_fields(nil, _, :desc), do: true
  defp compare_fields(_, nil, :desc), do: false
  defp compare_fields(field1, field2, :asc), do: field1 < field2
  defp compare_fields(field1, field2, :desc), do: field1 > field2
end
