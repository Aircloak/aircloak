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
    assert apply_function("round", [pow(10, 5000)]) === pow(10, 5000)
  end

  test "binary round" do
    assert well_typed?("round", [:real, :integer])
    assert well_typed?("round", [:integer, :integer])
    assert apply_function("round", [3.99, 1]) == 4.0
    assert apply_function("round", [3.91, 1]) == 3.9
    assert apply_function("round", [3.991, 2]) == 3.99
    assert apply_function("round", [3.99, 4]) == 3.99
    assert apply_function("round", [3, 1]) == 3
    assert apply_function("round", [pow(10, 5000), 1]) === pow(10, 5000)
  end

  test "trunc" do
    assert well_typed?("round", [:real])
    assert well_typed?("round", [:integer])
    assert apply_function("trunc", [3.99]) == 3
    assert apply_function("trunc", [-3.99]) == -3
    assert apply_function("trunc", [3]) == 3
    assert apply_function("trunc", [pow(10, 5000)]) === pow(10, 5000)
  end

  test "binary trunc" do
    assert well_typed?("round", [:real, :integer])
    assert well_typed?("round", [:integer, :integer])
    assert apply_function("trunc", [3.99, 1]) == 3.9
    assert apply_function("trunc", [-3.99, 1]) == -3.9
    assert apply_function("trunc", [3.99, 2]) == 3.99
    assert apply_function("trunc", [3.99, 4]) == 3.99
    assert apply_function("trunc", [3, 4]) == 3
    assert apply_function("trunc", [pow(10, 5000), 1]) === pow(10, 5000)
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

  test "||" do
    assert well_typed?("||", [:text, :text])
    assert apply_function("||", ["a ", "string"]) == "a string"
  end

  for function <- ~w(year month day weekday) do
    test function do
      assert well_typed?(unquote(function), [:timestamp])
      assert well_typed?(unquote(function), [:date])
      refute well_typed?(unquote(function), [:time])
    end
  end

  for function <- ~w(hour minute second) do
    test function do
      assert well_typed?(unquote(function), [:timestamp])
      refute well_typed?(unquote(function), [:date])
      assert well_typed?(unquote(function), [:time])
    end
  end

  for function <- ~w(* / + - ^) do
    test "#{function} typing" do
      assert well_typed?(unquote(function), [:integer, :integer])
      assert well_typed?(unquote(function), [:real, :real])
      refute well_typed?(unquote(function), [:text, :integer])
    end
  end

  test "% typing" do
    assert well_typed?("%", [:integer, :integer])
    refute well_typed?("%", [:real, :real])
  end

  test "any function with one of the arguments being :*", do:
    assert apply_function("whatever", [1, :*, "thing"]) == :*

  test "typechecking a nested function call" do
    assert Function.well_typed?({:function, "avg", [{:function, "abs", nil}]})
    refute Function.well_typed?({:function, "avg", [{:function, "concat", nil}]})
  end

  test "cast to integer typing" do
    assert well_typed?({"cast", :integer}, [:text])
    assert well_typed?({"cast", :integer}, [:boolean])
    assert well_typed?({"cast", :integer}, [:real])
    assert well_typed?({"cast", :integer}, [:integer])
    refute well_typed?({"cast", :integer}, [:timestamp])
    refute well_typed?({"cast", :integer}, [:date])
    refute well_typed?({"cast", :integer}, [:time])
  end

  test "cast to integer" do
    assert apply_function({"cast", :integer}, [123]) === 123
    assert apply_function({"cast", :integer}, [123.0]) === 123
    assert apply_function({"cast", :integer}, [123.1]) === 123
    assert apply_function({"cast", :integer}, [123.9]) === 124
    assert apply_function({"cast", :integer}, ["123"]) === 123
    assert apply_function({"cast", :integer}, ["-123"]) === -123
    assert apply_function({"cast", :integer}, ["123and some additional symbols"]) === 123
    assert apply_function({"cast", :integer}, [true]) === 1
    assert apply_function({"cast", :integer}, [false]) === 0
  end

  test "cast to real typing" do
    assert well_typed?({"cast", :real}, [:text])
    assert well_typed?({"cast", :real}, [:boolean])
    assert well_typed?({"cast", :real}, [:real])
    assert well_typed?({"cast", :real}, [:integer])
    refute well_typed?({"cast", :real}, [:timestamp])
    refute well_typed?({"cast", :real}, [:date])
    refute well_typed?({"cast", :real}, [:time])
  end

  test "cast to real" do
    assert apply_function({"cast", :real}, [123]) === 123.0
    assert apply_function({"cast", :real}, [pow(10, 400)]) === nil
    assert apply_function({"cast", :real}, [123.123]) === 123.123
    assert apply_function({"cast", :real}, ["123"]) === 123.0
    assert apply_function({"cast", :real}, ["-123"]) === -123.0
    assert apply_function({"cast", :real}, ["123.123"]) === 123.123
    assert apply_function({"cast", :real}, ["123.123and some additional symbols"]) === 123.123
    assert apply_function({"cast", :real}, [true]) === 1.0
    assert apply_function({"cast", :real}, [false]) === 0.0
  end

  test "cast to text typing" do
    assert well_typed?({"cast", :text}, [:text])
    assert well_typed?({"cast", :text}, [:boolean])
    assert well_typed?({"cast", :text}, [:real])
    assert well_typed?({"cast", :text}, [:integer])
    assert well_typed?({"cast", :text}, [:timestamp])
    assert well_typed?({"cast", :text}, [:date])
    assert well_typed?({"cast", :text}, [:time])
  end

  test "cast to text" do
    assert apply_function({"cast", :text}, [123]) === "123"
    assert apply_function({"cast", :text}, [123.123]) === "123.123"
    assert apply_function({"cast", :text}, ["123"]) === "123"
    assert apply_function({"cast", :text}, [true]) === "TRUE"
    assert apply_function({"cast", :text}, [false]) === "FALSE"
    assert apply_function({"cast", :text}, [%Timex.DateTime{
      year: 2015, month: 1, day: 2, hour: 3, minute: 4, second: 5, timezone: Timex.Timezone.get(:utc)
    }]) === "2015-01-02 03:04:05"
  end

  test "cast to boolean typing" do
    assert well_typed?({"cast", :boolean}, [:text])
    assert well_typed?({"cast", :boolean}, [:boolean])
    assert well_typed?({"cast", :boolean}, [:real])
    assert well_typed?({"cast", :boolean}, [:integer])
    refute well_typed?({"cast", :boolean}, [:timestamp])
    refute well_typed?({"cast", :boolean}, [:date])
    refute well_typed?({"cast", :boolean}, [:time])
  end

  test "cast to boolean" do
    assert apply_function({"cast", :boolean}, [1]) === true
    assert apply_function({"cast", :boolean}, [0]) === false
    assert apply_function({"cast", :boolean}, [123]) === true
    assert apply_function({"cast", :boolean}, [0.01]) === false
    assert apply_function({"cast", :boolean}, [0.9]) === true
    assert apply_function({"cast", :boolean}, ["tRuE"]) === true
    assert apply_function({"cast", :boolean}, ["fAlSe"]) === false
    assert apply_function({"cast", :boolean}, ["Bob"]) === nil
    assert apply_function({"cast", :boolean}, [true]) === true
    assert apply_function({"cast", :boolean}, [false]) === false
  end

  test "cast to timestamp typing" do
    assert well_typed?({"cast", :timestamp}, [:text])
    refute well_typed?({"cast", :timestamp}, [:boolean])
    refute well_typed?({"cast", :timestamp}, [:real])
    refute well_typed?({"cast", :timestamp}, [:integer])
    assert well_typed?({"cast", :timestamp}, [:timestamp])
    refute well_typed?({"cast", :timestamp}, [:date])
    refute well_typed?({"cast", :timestamp}, [:time])
  end

  test "cast to timestamp" do
    time = %Timex.DateTime{
      year: 2015, month: 1, day: 2, hour: 3, minute: 4, second: 5, timezone: Timex.Timezone.get(:utc)
    }
    assert apply_function({"cast", :timestamp}, [time]) === time
    assert apply_function({"cast", :timestamp}, [Timex.format!(time, "{ISOz}")]) === time
    assert apply_function({"cast", :timestamp}, ["some string"]) === nil
  end

  test "cast to time typing" do
    assert well_typed?({"cast", :time}, [:text])
    refute well_typed?({"cast", :time}, [:boolean])
    refute well_typed?({"cast", :time}, [:real])
    refute well_typed?({"cast", :time}, [:integer])
    assert well_typed?({"cast", :time}, [:timestamp])
    refute well_typed?({"cast", :time}, [:date])
    assert well_typed?({"cast", :time}, [:time])
  end

  test "cast to time" do
    time = %Timex.DateTime{
      year: 2015, month: 1, day: 2, hour: 3, minute: 4, second: 5, timezone: Timex.Timezone.get(:utc)
    }
    assert apply_function({"cast", :time}, [time]) === %{time | year: 0, month: 0, day: 0}
    assert apply_function({"cast", :time}, ["12:00:23"]) === %Timex.DateTime{
      hour: 12, minute: 0, second: 23, timezone: Timex.Timezone.get(:utc)
    }
    assert apply_function({"cast", :time}, ["some string"]) === nil
  end

  test "cast to date typing" do
    assert well_typed?({"cast", :date}, [:text])
    refute well_typed?({"cast", :date}, [:boolean])
    refute well_typed?({"cast", :date}, [:real])
    refute well_typed?({"cast", :date}, [:integer])
    assert well_typed?({"cast", :date}, [:timestamp])
    assert well_typed?({"cast", :date}, [:date])
    refute well_typed?({"cast", :date}, [:time])
  end

  test "cast to date" do
    time = %Timex.DateTime{
      year: 2015, month: 1, day: 2, hour: 3, minute: 4, second: 5, millisecond: 6,
      timezone: Timex.Timezone.get(:utc)
    }
    assert apply_function({"cast", :date}, [time]) === %{time | hour: 0, minute: 0, second: 0, millisecond: 0}
    assert apply_function({"cast", :date}, ["2016-01-02"]) === %Timex.DateTime{
      year: 2016, month: 1, day: 2, timezone: Timex.Timezone.get(:utc)
    }
    assert apply_function({"cast", :date}, ["some string"]) === nil
  end

  for type <- [:text, :boolean, :real, :integer, :timestamp, :date, :time] do
    test "casting nil to #{type}" do
      assert apply_function({"cast", unquote(type)}, [nil]) === nil
    end
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
