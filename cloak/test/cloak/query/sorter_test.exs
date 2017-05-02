defmodule Cloak.Query.SorterTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Sorter

  Enum.each([
    {"a string", :asc},
    {"a string", :desc},
    {1, :asc},
    {1, :desc},
    {nil, :asc},
    {nil, :desc},
  ], fn {other_value, order} ->
    test ":* is ordered after #{other_value} when the order is #{order}" do
      assert Sorter.order_rows(
        [[:*], [unquote(other_value)], [:*]],
        [:a],
        [{:a, unquote(:order)}]
      ) == [[unquote(other_value)], [:*], [:*]]
    end
  end)

  test "nil is ordered after present values and before anonymized values", do:
    assert Sorter.order_rows(
      [[nil], [:*], ["aaa"], [nil]],
      [:a], [{:a, :asc}]
    ) == [["aaa"], [nil], [nil], [:*]]

  test "rows with :* are ordered after other rows in the default order", do:
    assert Sorter.order_rows(
      [[:*], ["some value"], [:*]],
      [:a], [{:a, :asc}]
    ) == [["some value"], [:*], [:*]]
end
