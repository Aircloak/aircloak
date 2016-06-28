defmodule Cloak.Query.SorterTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Row
  alias Cloak.Query.Sorter

  defp list_to_rows(values) do
    Enum.map(values, &Row.new([:column_1], [&1]))
  end

  defp rows_to_list(rows), do: Enum.map(rows, &Row.values/1)

  Enum.each([
    {"a string", :asc},
    {"a string", :desc},
    {1, :asc},
    {1, :desc},
    {nil, :asc},
    {nil, :desc},
  ], fn {other_value, order} ->
    test ":* is ordered after #{other_value} when the order is #{order}" do
      rows = [:*, unquote(other_value), :*] |> list_to_rows()
      query = %{columns: [], order_by: [{:column_1, unquote(order)}]}

      ordered = Sorter.order_rows(rows, query) |> rows_to_list()

      assert ordered == [[unquote(other_value)], [:*], [:*]]
    end
  end)

  test "nil is ordered after present values and before anonymized values" do
    rows = [nil, :*, "aaa", nil] |> list_to_rows()
    query = %{columns: [], order_by: [{:column_1, :asc}]}
    ordered = Sorter.order_rows(rows, query) |> rows_to_list()
    assert ordered == [["aaa"], [nil], [nil], [:*]]
  end
end
