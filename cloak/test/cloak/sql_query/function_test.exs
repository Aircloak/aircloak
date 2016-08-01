defmodule Cloak.SqlQuery.Function.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Function

  test "sqrt" do
    assert_in_delta(Function.apply(3, {:function, "sqrt", nil}), 1.73, 0.1)
  end
end
