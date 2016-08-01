defmodule Cloak.SqlQuery.Function.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Function

  test "sqrt", do:
    assert_in_delta(apply_function("sqrt", 3), 1.73, 0.1)

  test "floor" do
    assert apply_function("floor", 3.99) == 3
    assert apply_function("floor", 3.01) == 3
  end

  test "ceil" do
    assert apply_function("ceil", 3.99) == 4
    assert apply_function("ceil", 3.01) == 4
    assert apply_function("ceiling", 3.99) == 4
    assert apply_function("ceiling", 3.01) == 4
  end

  test "abs" do
    assert apply_function("abs", 1.2) == 1.2
    assert apply_function("abs", -1.2) == 1.2
    assert apply_function("abs", 1) == 1
    assert apply_function("abs", -1) == 1
  end

  defp apply_function(name, arg), do:
    Function.apply(arg, {:function, name, nil})
end
