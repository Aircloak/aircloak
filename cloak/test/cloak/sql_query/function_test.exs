defmodule Cloak.SqlQuery.Function.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Function

  test "sqrt" do
    assert_in_delta(Function.apply(3, {:function, "sqrt", nil}), 1.73, 0.1)
  end

  test "floor" do
    assert Function.apply(3.99, {:function, "floor", nil}) == 3
  end
end
