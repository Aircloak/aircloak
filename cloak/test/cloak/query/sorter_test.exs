defmodule Cloak.Query.SorterTest do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Query
  alias Cloak.Query.{Sorter, Result}

  defp list_to_buckets(values), do: for value <- values, do: %{row: [value]}

  defp sort(rows, query) do
    %Result{rows: ordered_rows} = Sorter.order(%Result{rows: rows}, query)
    for %{row: [value]} <- ordered_rows, do: value
  end

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
      query = %Query{columns: [], order_by: [{0, unquote(order)}]}

      assert sort(rows, query) == [unquote(other_value), :*, :*]
    end
  end)

  test "nil is ordered after present values and before anonymized values" do
    rows = [nil, :*, "aaa", nil] |> list_to_buckets()
    query = %Query{columns: [], order_by: [{0, :asc}]}
    assert sort(rows, query) == ["aaa", nil, nil, :*]
  end

  test "rows with :* are ordered after other rows in the default order" do
    rows = [:*, "some value", :*] |> list_to_buckets()
    query = %Query{columns: [], order_by: []}
    assert sort(rows, query) == ["some value", :*, :*]
  end
end
