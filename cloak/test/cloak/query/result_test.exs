defmodule Cloak.Query.Result.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.Result

  Enum.each([
    {"a string", :asc},
    {"a string", :desc},
    {1, :asc},
    {1, :desc},
  ], fn {other_value, order} ->
    test ":* is ordered after #{other_value} when the order is #{order}" do
      rows = [[:*], [unquote(other_value)], [:*]]
      query = %{columns: ["something"], order_by: [{"something", unquote(order)}]}

      assert Result.apply_order(rows, query) == [[unquote(other_value)], [:*], [:*]]
    end
  end)
end
