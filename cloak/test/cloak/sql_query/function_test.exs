defmodule Cloak.SqlQuery.Function.Test do
  use ExUnit.Case, async: true

  alias Cloak.SqlQuery.Function

  test "sqrt", do:
    assert_in_delta(apply_function("sqrt", [3]), 1.73, 0.1)

  test "floor" do
    assert apply_function("floor", [3.99]) == 3
    assert apply_function("floor", [3.01]) == 3
    assert apply_function("floor", [-3.99]) == -4
  end

  test "ceil" do
    assert apply_function("ceil", [3.99]) == 4
    assert apply_function("ceil", [3.01]) == 4
    assert apply_function("ceiling", [3.99]) == 4
    assert apply_function("ceiling", [3.01]) == 4
  end

  test "abs" do
    assert apply_function("abs", [1.2]) == 1.2
    assert apply_function("abs", [-1.2]) == 1.2
    assert apply_function("abs", [1]) == 1
    assert apply_function("abs", [-1]) == 1
  end

  test "round" do
    assert apply_function("round", [3.99]) == 4
    assert apply_function("round", [3.01]) == 3
  end

  test "trunc" do
    assert apply_function("trunc", [3.99]) == 3
    assert apply_function("trunc", [-3.99]) == -3
  end

  test "div" do
    assert apply_function("div", [12, 3]) == 4
    assert apply_function("div", [13, 3]) == 4
  end

  test "mod", do:
    assert apply_function("mod", [13, 3]) == 1

  test "pow", do:
    assert apply_function("pow", [2, 3]) == 8

  test "any function with one of the arguments being :*", do:
    assert apply_function("whatever", [1, :*, "thing"]) == :*

  defp apply_function(name, args), do:
    Function.apply(args, {:function, name, nil})
end
