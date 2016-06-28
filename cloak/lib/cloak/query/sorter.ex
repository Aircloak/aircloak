defmodule Cloak.Query.Sorter do
  @moduledoc "Sorts rows according to the query specification."

  alias Cloak.DataSource.Row
  alias Cloak.SqlQuery


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

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
