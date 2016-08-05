defmodule Cloak.Query.SorterTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Sorter

  defp list_to_buckets(values), do: for value <- values, do: %{row: [value]}
  defp buckets_to_list(rows), do: for %{row: [value]} <- rows, do: value

  Enum.each([
    {"a string", :asc},
    {"a string", :desc},
    {1, :asc},
    {1, :desc},
    {nil, :asc},
    {nil, :desc},
  ], fn {other_value, order} ->
    test ":* is ordered after #{other_value} when the order is #{order}" do
      rows = [:*, unquote(other_value), :*] |> list_to_buckets()
      query = %{columns: [], order_by: [{0, unquote(order)}]}

      ordered = Sorter.order(rows, query) |> buckets_to_list()

      assert ordered == [unquote(other_value), :*, :*]
    end
  end)

  test "nil is ordered after present values and before anonymized values" do
    rows = [nil, :*, "aaa", nil] |> list_to_buckets()
    query = %{columns: [], order_by: [{0, :asc}]}
    ordered = Sorter.order(rows, query) |> buckets_to_list()
    assert ordered == ["aaa", nil, nil, :*]
  end

  test "rows with :* are ordered after other rows in the default order" do
    rows = [:*, "some value", :*] |> list_to_buckets()
    query = %{columns: [], order_by: []}
    ordered = Sorter.order(rows, query) |> buckets_to_list()
    assert ordered == ["some value", :*, :*]
  end
end
