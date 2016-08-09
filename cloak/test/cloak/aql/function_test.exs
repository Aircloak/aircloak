defmodule Cloak.Aql.Function.Test do
  require Integer
  use ExUnit.Case, async: true

  alias Cloak.Aql.{Column, Function}

  test "sqrt", do:
    assert_in_delta(apply_function("sqrt", [3]), 1.73, 0.1)

  for function <- ~w(floor ceil ceiling) do
    test "#{function} argument types" do
      assert well_typed?(unquote(function), [:integer])
      assert well_typed?(unquote(function), [:real])
    end
  end

  test "floor" do
    assert apply_function("floor", [3.99]) === 3
    assert apply_function("floor", [3.01]) === 3
    assert apply_function("floor", [-3.99]) === -4
    assert apply_function("floor", [3]) === 3
    assert apply_function("floor", [pow(10, 5000)]) === pow(10, 5000)
  end

  test "ceil" do
    assert apply_function("ceil", [3.99]) === 4
    assert apply_function("ceil", [3.01]) === 4
    assert apply_function("ceil", 3) === 3
    assert apply_function("ceiling", [3.99]) === 4
    assert apply_function("ceiling", [3.01]) === 4
    assert apply_function("ceiling", 3) === 3
    assert apply_function("ceil", [pow(10, 5000)]) === pow(10, 5000)
  end

  test "abs" do
    assert apply_function("abs", [1.2]) == 1.2
    assert apply_function("abs", [-1.2]) == 1.2
    assert apply_function("abs", [1]) == 1
    assert apply_function("abs", [-1]) == 1
  end

  test "round" do
    assert well_typed?("round", [:real])
    assert well_typed?("round", [:integer])
    assert apply_function("round", [3.99]) == 4
    assert apply_function("round", [3.01]) == 3
    assert apply_function("round", [3]) == 3
  end

  test "binary round" do
    assert well_typed?("round", [:real, :integer])
    assert well_typed?("round", [:integer, :integer])
    assert apply_function("round", [3.99, 1]) == 4.0
    assert apply_function("round", [3.91, 1]) == 3.9
    assert apply_function("round", [3.991, 2]) == 3.99
    assert apply_function("round", [3.99, 4]) == 3.99
    assert apply_function("round", [3, 1]) == 3
  end

  test "trunc" do
    assert well_typed?("round", [:real])
    assert well_typed?("round", [:integer])
    assert apply_function("trunc", [3.99]) == 3
    assert apply_function("trunc", [-3.99]) == -3
    assert apply_function("trunc", [3]) == 3
  end

  test "binary trunc" do
    assert well_typed?("round", [:real, :integer])
    assert well_typed?("round", [:integer, :integer])
    assert apply_function("trunc", [3.99, 1]) == 3.9
    assert apply_function("trunc", [-3.99, 1]) == -3.9
    assert apply_function("trunc", [3.99, 2]) == 3.99
    assert apply_function("trunc", [3.99, 4]) == 3.99
    assert apply_function("trunc", [3, 4]) == 3
  end

  test "div" do
    assert apply_function("div", [12, 3]) == 4
    assert apply_function("div", [13, 3]) == 4
  end

  test "mod", do:
    assert apply_function("mod", [13, 3]) == 1

  test "pow", do:
    assert apply_function("pow", [2, 3]) == 8

  test "length" do
    assert well_typed?("length", [:text])
    assert apply_function("length", ["a string"]) == 8
  end

  test "left" do
    assert well_typed?("left", [:text, :integer])
    assert apply_function("left", ["a string", 2]) == "a "
    assert apply_function("left", ["a string", -2]) == "a stri"
    assert apply_function("left", ["a string", -10]) == ""
  end

  test "right" do
    assert well_typed?("right", [:text, :integer])
    assert apply_function("right", ["a string", 2]) == "ng"
    assert apply_function("right", ["a string", -2]) == "string"
  end

  test "lower" do
    assert well_typed?("lower", [:text])
    assert well_typed?("lcase", [:text])
    assert apply_function("lower", ["A sTrinG"]) == "a string"
    assert apply_function("lcase", ["A sTrinG"]) == "a string"
  end

  test "upper" do
    assert well_typed?("upper", [:text])
    assert well_typed?("ucase", [:text])
    assert apply_function("upper", ["A sTrinG"]) == "A STRING"
    assert apply_function("ucase", ["A sTrinG"]) == "A STRING"
  end

  test "btrim" do
    assert well_typed?("btrim", [:text])
    assert well_typed?("btrim", [:text, :text])
    assert apply_function("btrim", ["  a string "]) == "a string"
    assert apply_function("btrim", ["xyxa stringxyx", "xy"]) == "a string"
  end

  test "ltrim" do
    assert well_typed?("ltrim", [:text])
    assert well_typed?("ltrim", [:text, :text])
    assert apply_function("ltrim", ["  a string "]) == "a string "
    assert apply_function("ltrim", ["xyxa stringxyx", "xy"]) == "a stringxyx"
  end

  test "rtrim" do
    assert well_typed?("rtrim", [:text])
    assert well_typed?("rtrim", [:text, :text])
    assert apply_function("rtrim", ["  a string "]) == "  a string"
    assert apply_function("rtrim", ["xyxa stringxyx", "xy"]) == "xyxa string"
  end

  test "substring" do
    assert well_typed?("substring", [:text, :integer])
    assert well_typed?("substring", [:text, :integer, :integer])
    assert well_typed?("substring_for", [:text, :integer])
    assert apply_function("substring", ["a string", 3]) == "string"
    assert apply_function("substring", ["a string", 3, 2]) == "st"
    assert apply_function("substring", ["a string", -3, 2]) == ""
    assert apply_function("substring", ["a string", -1, 4]) == "a "
    assert apply_function("substring", ["a string", 3, -2]) == ""
    assert apply_function("substring_for", ["a string", 3]) == "a s"
  end

  test "concat" do
    assert well_typed?("concat", [:text])
    assert well_typed?("concat", [:text, :text])
    assert well_typed?("concat", [:text, :text, :text, :text, :text])
    refute well_typed?("concat", [:text, :text, :integer, :text, :text])
    assert apply_function("concat", ["a", " ", "string"]) == "a string"
  end

  test "any function with one of the arguments being :*", do:
    assert apply_function("whatever", [1, :*, "thing"]) == :*

  test "typechecking a nested function call" do
    assert Function.well_typed?({:function, "avg", [{:function, "abs", nil}]})
    refute Function.well_typed?({:function, "avg", [{:function, "concat", nil}]})
  end

  defp apply_function(name, args), do:
    Function.apply(args, {:function, name, nil})

  defp well_typed?(name, types), do:
    Function.well_typed?({:function, name, Enum.map(types, &Column.constant(&1, nil))})

  defp pow(_, 0), do: 1
  defp pow(x, n) when Integer.is_odd(n), do: x * pow(x, n - 1)
  defp pow(x, n) do
    result = pow(x, div(n, 2))
    result * result
  end
end
