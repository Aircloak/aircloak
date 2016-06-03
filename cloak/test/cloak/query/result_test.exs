defmodule Cloak.Query.Result.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.Result
  use Cloak.Type

  Enum.each([
    {"a string", :asc},
    {"a string", :desc},
    {1, :asc},
    {1, :desc},
  ], fn {other_value, order} ->
    test ":* is ordered after #{other_value} when the order is #{order}" do
      buckets = [[:*], [unquote(other_value)], [:*]] |> Enum.map(&bucket(property: &1))
      query = %{columns: ["something"], order_by: [{"something", unquote(order)}]}

      ordered = Result.apply_order(buckets, query) |> Enum.map(&bucket(&1, :property))

      assert ordered == [[unquote(other_value)], [:*], [:*]]
    end
  end)
end
