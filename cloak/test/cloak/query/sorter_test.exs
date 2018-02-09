defmodule Cloak.Query.SorterTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Sorter

  Enum.each([
    {"a string", :asc, :nulls_last},
    {"a string", :desc, :nulls_first},
    {1, :asc, :nulls_last},
    {1, :desc, :nulls_first},
    {nil, :asc, :nulls_last},
    {nil, :desc, :nulls_first},
  ], fn {other_value, order, nulls} ->
    test ":* is ordered after #{other_value} when the order is #{order}" do
      assert Sorter.order_rows(
        [[:*], [unquote(other_value)], [:*]],
        [:a],
        [{:a, unquote(:order), unquote(nulls)}]
      ) == [[unquote(other_value)], [:*], [:*]]
    end
  end)

  for order <- [:asc, :desc] do
    test "nil is ordered after present values with nulls last and #{order} and before anonymized values", do:
      assert Sorter.order_rows(
        [[nil], [:*], ["aaa"], [nil]],
        [:a], [{:a, unquote(order), :nulls_last}]
      ) == [["aaa"], [nil], [nil], [:*]]

    test "nil is ordered before present values with nulls first and #{order} and before anonymized values", do:
      assert Sorter.order_rows(
        [[nil], [:*], ["aaa"], [nil]],
        [:a], [{:a, unquote(order), :nulls_first}]
      ) == [[nil], [nil], ["aaa"], [:*]]
  end

  test "rows with :* are ordered after other rows in the default order", do:
    assert Sorter.order_rows(
      [[:*], ["some value"], [:*]],
      [:a], [{:a, :asc, :nulls_last}]
    ) == [["some value"], [:*], [:*]]

  test "regular values are ordered naturally", do:
    assert Sorter.order_rows(
      [["b"], ["a"], ["c"]],
      [:a], [{:a, :asc, :nulls_last}]
    ) == [["a"], ["b"], ["c"]]
end
