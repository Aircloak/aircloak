defmodule Cloak.SqlQuery.Function.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Function

  test "sqrt" do
    assert_in_delta(Function.apply(3, {:function, "sqrt", nil}), 1.73, 0.1)
  end

  test "floor" do
    assert Function.apply(3.99, {:function, "floor", nil}) == 3
    assert Function.apply(3.01, {:function, "floor", nil}) == 3
  end

  test "ceil" do
    assert Function.apply(3.99, {:function, "ceil", nil}) == 4
    assert Function.apply(3.01, {:function, "ceil", nil}) == 4
    assert Function.apply(3.99, {:function, "ceiling", nil}) == 4
    assert Function.apply(3.01, {:function, "ceiling", nil}) == 4
  end

  test "abs" do
    assert Function.apply(1.2, {:function, "abs", nil}) == 1.2
    assert Function.apply(-1.2, {:function, "abs", nil}) == 1.2
    assert Function.apply(1, {:function, "abs", nil}) == 1
    assert Function.apply(-1, {:function, "abs", nil}) == 1
  end
end
