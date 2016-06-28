defmodule Cloak.Query.Result do
  @moduledoc "Contains functions for converting buckets to a columns/rows representation."

  import Cloak.Type
  alias Cloak.DataSource.Row
  alias Cloak.SqlQuery


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Converts a list of buckets into aggregate rows for reporting.
  The consuming client will still have to expand the rows to mimic normal SQL
  where individual rows are produced.
  """
  @spec buckets([Row.t], SqlQuery.t) :: [Bucket.t]
  def buckets(rows, query) do
    for row <- rows do
      %{
        row: Row.values(row, query.columns),
        occurrences:
          if query[:implicit_count] do
            Row.value(row, {:function, "count", :*})
          else
            1
          end
      }
    end
  end

  @doc "Groups the data values to be aggregated by the selected property and the reported users."
  @spec group_by_property_and_users([Row.t], SqlQuery.t) :: GroupedRows.t
  def group_by_property_and_users(rows, query) do
    Enum.reduce(rows, %{}, fn(row, accumulator) ->
      property = for column <- query.property, do: Row.value(row, column)
      user = user_id(row)
      Map.update(accumulator, property, %{user => [row]}, fn (user_values_map) ->
        Map.update(user_values_map, user, [row], &([row | &1]))
      end)
    end)
  end

  @doc "Sorts the rows in the order defined in the query."
  @spec order_rows([Row.t], SqlQuery.t) :: [Row.t]
  def order_rows(rows, %{order_by: order_list}) do
    Enum.sort(rows, fn(row1, row2) ->
      compare_rows(row1, row2, order_list)
    end)
  end
  def order_rows(rows, _), do: rows


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp user_id(row) do
    Row.value(row, hd(row.columns))
  end

  defp compare_rows(row1, row2, []), do: row1 < row2
  defp compare_rows(row1, row2, [{column, direction} | remaining_order]) do
    field1 = Row.value(row1, column)
    field2 = Row.value(row2, column)
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
