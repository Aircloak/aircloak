defmodule Cloak.Query.Sorter do
  @moduledoc "Sorting of rows."

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Sorts the rows per given sort expressions.

  The `mapper` function can be optionally provided to map the row before comparing it.
  """
  @spec order_rows(
          Enumerable.t(),
          [Expression.t()],
          [{Expression.t(), :asc | :desc}]
        ) :: Enumerable.t()
  def order_rows(rows, columns, order_by)
  def order_rows(rows, _columns, []), do: rows

  def order_rows(rows, columns, order_by) do
    order_by_indices =
      Enum.map(order_by, fn {expression, direction, nulls} ->
        index = Enum.find_index(columns, &(&1 == expression))
        true = index != nil
        {index, direction, nulls}
      end)

    Enum.sort(rows, &compare_rows(&1, &2, order_by_indices))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compare_rows(bucket1, bucket2, order_by_indices) when is_map(bucket1) and is_map(bucket2),
    do: compare_rows(bucket1.row, bucket2.row, order_by_indices)

  defp compare_rows(row1, row2, []) when is_list(row1) and is_list(row2) do
    cond do
      Enum.member?(row1, :*) -> false
      Enum.member?(row2, :*) -> true
      true -> row1 < row2
    end
  end

  defp compare_rows(row1, row2, [{index, direction, nulls} | remaining_order]) when is_list(row1) and is_list(row2) do
    field1 = Enum.at(row1, index)
    field2 = Enum.at(row2, index)

    case field1 === field2 do
      true -> compare_rows(row1, row2, remaining_order)
      false -> compare_fields(field1, field2, direction, nulls)
    end
  end

  defp compare_fields(:*, _, _, _), do: false
  defp compare_fields(_, :*, _, _), do: true
  defp compare_fields(nil, _, _, :nulls_last), do: false
  defp compare_fields(_, nil, _, :nulls_last), do: true
  defp compare_fields(nil, _, _, :nulls_first), do: true
  defp compare_fields(_, nil, _, :nulls_first), do: false
  defp compare_fields(nil, _, :asc, :nulls_natural), do: false
  defp compare_fields(_, nil, :asc, :nulls_natural), do: true
  defp compare_fields(nil, _, :desc, :nulls_natural), do: true
  defp compare_fields(_, nil, :desc, :nulls_natural), do: false
  defp compare_fields(field1, field2, :asc, _), do: Cloak.Data.lt_eq(field1, field2)
  defp compare_fields(field1, field2, :desc, _), do: Cloak.Data.lt_eq(field2, field1)
end
