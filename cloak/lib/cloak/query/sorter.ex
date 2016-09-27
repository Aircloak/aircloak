defmodule Cloak.Query.Sorter do
  @moduledoc "Sorts buckets according to the query specification."

  alias Cloak.Query.Aggregator
  alias Cloak.Aql.Query


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Sorts the buckets in the order defined in the query."
  @spec order([Aggregator.bucket], Query.t) :: [Aggregator.bucket]
  def order(buckets, %Query{order_by: order_list}) do
    Enum.sort(buckets, fn(%{row: row1}, %{row: row2}) ->
      compare_rows(row1, row2, order_list)
    end)
  end
  def order(rows, _), do: rows


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
  defp compare_rows(row1, row2, [{index, direction} | remaining_order]) do
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
  defp compare_fields(%NaiveDateTime{} = field1, %NaiveDateTime{} = field2, direction) do
    compare_fields(NaiveDateTime.to_erl(field1), NaiveDateTime.to_erl(field2), direction)
  end
  defp compare_fields(field1, field2, :asc), do: field1 < field2
  defp compare_fields(field1, field2, :desc), do: field1 > field2
end
