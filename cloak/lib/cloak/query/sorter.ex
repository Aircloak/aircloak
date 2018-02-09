defmodule Cloak.Query.Sorter do
  @moduledoc "Sorting of rows."


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Sorts the rows per given sort expressions.

  The `mapper` function can be optionally provided to map the row before comparing it.
  """
  @spec order_rows(Enumerable.t, [Expression.t], [{Expression.t, :asc | :desc}], ((any) -> [Cloak.DataSource.field]))
    :: Enumerable.t
  def order_rows(rows, columns, query, mapper \\ &(&1))
  def order_rows(rows, _columns, [], _mapper), do: rows
  def order_rows(rows, columns, order_by, mapper) do
    order_by_indices = Enum.map(order_by,
      fn({expression, direction, nulls}) ->
        index = Enum.find_index(columns, &(&1 == expression))
        true = (index != nil)
        {index, direction, nulls}
      end
    )

    Enum.sort(rows, &compare_rows(mapper.(&1), mapper.(&2), order_by_indices))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compare_rows(row1, row2, []) do
    cond do
      Enum.member?(row1, :*) -> false
      Enum.member?(row2, :*) -> true
      true -> row1 < row2
    end
  end
  defp compare_rows(row1, row2, [{index, direction, _nulls} | remaining_order]) do
    field1 = Enum.at(row1, index)
    field2 = Enum.at(row2, index)
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
  defp compare_fields(field1, field2, :asc), do: Cloak.Data.lt_eq(field1, field2)
  defp compare_fields(field1, field2, :desc), do: Cloak.Data.lt_eq(field2, field1)
end
