defmodule Cloak.Query.Result.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.Result
  use Cloak.Type

  Enum.each([
    {"a string", :asc},
    {"a string", :desc},
    {1, :asc},
    {1, :desc},
    {nil, :asc},
    {nil, :desc},
  ], fn {other_value, order} ->
    test ":* is ordered after #{other_value} when the order is #{order}" do
      buckets = [[:*], [unquote(other_value)], [:*]] |> Enum.map(&bucket(property: &1))
      query = %{columns: [], order_by: [{0, unquote(order)}]}

      ordered = Result.apply_order(buckets, query) |> Enum.map(&bucket(&1, :property))

      assert ordered == [[unquote(other_value)], [:*], [:*]]
    end
  end)

  test "nil is ordered after present values and before anonymized values" do
    buckets = [[nil], [:*], ["aaa"], [nil]] |> Enum.map(&bucket(property: &1))
    query = %{columns: [], order_by: [{0, :asc}]}
    ordered = Result.apply_order(buckets, query) |> Enum.map(&bucket(&1, :property))
    assert ordered == [["aaa"], [nil], [nil], [:*]]
  end
end
