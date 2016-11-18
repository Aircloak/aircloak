defmodule Cloak.Aql.Function.Test do
  require Integer
  use ExUnit.Case, async: true

  alias Cloak.Aql.{Column, Function}
  alias Timex.Duration

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
    assert apply_function("ceil", [3]) === 3
    assert apply_function("ceiling", [3.99]) === 4
    assert apply_function("ceiling", [3.01]) === 4
    assert apply_function("ceiling", [3]) === 3
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
      assert well_typed?(unquote(function), [:datetime])
      assert well_typed?(unquote(function), [:date])
      refute well_typed?(unquote(function), [:time])
    end
  end

  for function <- ~w(hour minute second) do
    test function do
      assert well_typed?(unquote(function), [:datetime])
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

  test "subtracting dates" do
    assert well_typed?("-", [:date, :date])
    assert return_type("-", [:date, :date]) == :interval
    assert apply_function("-", [~D[2015-01-30], ~D[2015-01-20]]) === Duration.from_days(10)
  end

  test "subtracting times" do
    assert well_typed?("-", [:time, :time])
    assert return_type("-", [:time, :time]) == :interval
    assert apply_function("-", [~T[10:20:00], ~T[10:00:00]]) === Duration.from_minutes(20)
  end

  test "subtracting datetimes" do
    assert well_typed?("-", [:datetime, :datetime])
    assert return_type("-", [:datetime, :datetime]) == :interval
    assert apply_function("-", [~N[2015-01-02 10:20:00], ~N[2015-01-01 10:00:00]]) ===
      Duration.parse!("P1DT20M")
  end

  test "date + interval" do
    assert well_typed?("+", [:date, :interval])
    assert return_type("+", [:date, :interval]) == :datetime
    assert apply_function("+", [~D[2015-01-01], Duration.parse!("P10DT10M")]) === ~N[2015-01-11 00:10:00]
  end

  test "time + interval" do
    assert well_typed?("+", [:time, :interval])
    assert return_type("+", [:time, :interval]) == :time
    assert apply_function("+", [~T[10:20:30], Duration.parse!("P10DT1H10M")]) === ~T[11:30:30]
  end

  test "datetime + interval" do
    assert well_typed?("+", [:datetime, :interval])
    assert return_type("+", [:datetime, :interval]) == :datetime
    assert apply_function("+", [~N[2015-01-01 10:20:30], Duration.parse!("P10DT10M")]) === ~N[2015-01-11 10:30:30]
  end

  for {type, value} <- %{time: ~T[10:20:30], date: ~D[2015-01-02], datetime: ~N[2015-01-02 10:20:30]} do
    @value value
    @interval Duration.parse!("P10DT10M")

    test "#{type} - interval" do
      assert well_typed?("-", [unquote(type), :interval])
      assert apply_function("-", [@value, @interval]) ===
        apply_function("+", [@value, Duration.scale(@interval, -1)])
    end

    test "interval - #{type} is ill-typed" do
      refute well_typed?("-", [:interval, unquote(type)])
    end

    test "interval + #{type}" do
      assert well_typed?("+", [:interval, unquote(type)])
      assert apply_function("+", [@interval, @value]) == apply_function("+", [@value, @interval])
    end
  end

  test "interval + interval" do
    assert well_typed?("+", [:interval, :interval])
    assert return_type("+", [:interval, :interval]) == :interval
    assert apply_function("+", [Duration.parse!("P10D"), Duration.parse!("PT10M")]) ===
      Duration.parse!("P10DT10M")
  end

  test "interval - interval" do
    assert well_typed?("-", [:interval, :interval])
    assert return_type("-", [:interval, :interval]) == :interval
    assert apply_function("-", [Duration.parse!("P10DT10M"), Duration.parse!("P1DT1M")]) ===
      Duration.parse!("P9DT9M")
  end

  test "interval * number" do
    assert return_type("*", [:interval, :integer]) == :interval
    assert return_type("*", [:integer, :interval]) == :interval
    assert return_type("*", [:interval, :real]) == :interval
    assert return_type("*", [:real, :interval]) == :interval
    assert apply_function("*", [Duration.parse!("P1DT1M"), 10]) === Duration.parse!("P10DT10M")
    assert apply_function("*", [10, Duration.parse!("P1DT1M")]) === Duration.parse!("P10DT10M")
    assert apply_function("*", [Duration.parse!("P10DT10M"), 0.5]) === Duration.parse!("P5DT5M")
  end

  test "interval / number" do
    assert return_type("/", [:interval, :integer]) == :interval
    assert return_type("/", [:interval, :real]) == :interval
    assert apply_function("/", [Duration.parse!("P10DT10M"), 2]) === Duration.parse!("P5DT5M")
    assert apply_function("/", [Duration.parse!("P10DT10M"), 0.5]) === Duration.parse!("P20DT20M")
  end

  test "number / interval is ill-typed" do
    refute well_typed?("/", [:integer, :interval])
    refute well_typed?("/", [:real, :interval])
  end

  test "% typing" do
    assert well_typed?("%", [:integer, :integer])
    refute well_typed?("%", [:real, :real])
  end

  test "any function with one of the arguments being :*", do:
    assert apply_function("whatever", [1, :*, "thing"]) == :*

  test "typechecking a nested function call" do
    assert Function.well_typed?({:function, "avg", [{:function, "abs", [Column.constant(:integer, 3)]}]})
    refute Function.well_typed?({:function, "avg", [{:function, "concat", []}]})
  end

  for function <- ~w(round trunc) do
    test "#{function} return type" do
      assert return_type(unquote(function), [:real]) == :integer
      assert return_type(unquote(function), [:real, :integer]) == :real
    end
  end

  test "cast to integer typing" do
    assert well_typed?({:cast, :integer}, [:text])
    assert well_typed?({:cast, :integer}, [:boolean])
    assert well_typed?({:cast, :integer}, [:real])
    assert well_typed?({:cast, :integer}, [:integer])
    refute well_typed?({:cast, :integer}, [:datetime])
    refute well_typed?({:cast, :integer}, [:date])
    refute well_typed?({:cast, :integer}, [:time])
    refute well_typed?({:cast, :integer}, [:interval])
  end

  test "cast to integer" do
    assert apply_function({:cast, :integer}, [123]) === 123
    assert apply_function({:cast, :integer}, [123.0]) === 123
    assert apply_function({:cast, :integer}, [123.1]) === 123
    assert apply_function({:cast, :integer}, [123.9]) === 124
    assert apply_function({:cast, :integer}, ["123"]) === 123
    assert apply_function({:cast, :integer}, ["-123"]) === -123
    assert apply_function({:cast, :integer}, ["123and some additional symbols"]) === 123
    assert apply_function({:cast, :integer}, [true]) === 1
    assert apply_function({:cast, :integer}, [false]) === 0
  end

  test "cast to real typing" do
    assert well_typed?({:cast, :real}, [:text])
    assert well_typed?({:cast, :real}, [:boolean])
    assert well_typed?({:cast, :real}, [:real])
    assert well_typed?({:cast, :real}, [:integer])
    refute well_typed?({:cast, :real}, [:datetime])
    refute well_typed?({:cast, :real}, [:date])
    refute well_typed?({:cast, :real}, [:time])
    refute well_typed?({:cast, :real}, [:interval])
  end

  test "cast to real" do
    assert apply_function({:cast, :real}, [123]) === 123.0
    assert apply_function({:cast, :real}, [pow(10, 400)]) === nil
    assert apply_function({:cast, :real}, [123.123]) === 123.123
    assert apply_function({:cast, :real}, ["123"]) === 123.0
    assert apply_function({:cast, :real}, ["-123"]) === -123.0
    assert apply_function({:cast, :real}, ["123.123"]) === 123.123
    assert apply_function({:cast, :real}, ["123.123and some additional symbols"]) === 123.123
    assert apply_function({:cast, :real}, [true]) === 1.0
    assert apply_function({:cast, :real}, [false]) === 0.0
  end

  test "cast to text typing" do
    assert well_typed?({:cast, :text}, [:text])
    assert well_typed?({:cast, :text}, [:boolean])
    assert well_typed?({:cast, :text}, [:real])
    assert well_typed?({:cast, :text}, [:integer])
    assert well_typed?({:cast, :text}, [:datetime])
    assert well_typed?({:cast, :text}, [:date])
    assert well_typed?({:cast, :text}, [:time])
    assert well_typed?({:cast, :text}, [:interval])
  end

  test "cast to text" do
    assert apply_function({:cast, :text}, [123]) === "123"
    assert apply_function({:cast, :text}, [123.123]) === "123.123"
    assert apply_function({:cast, :text}, ["123"]) === "123"
    assert apply_function({:cast, :text}, [true]) === "TRUE"
    assert apply_function({:cast, :text}, [false]) === "FALSE"
    assert apply_function({:cast, :text}, [~N[2015-01-02 03:04:05]]) === "2015-01-02 03:04:05"
    assert apply_function({:cast, :text}, [~T[03:04:05]]) === "03:04:05"
    assert apply_function({:cast, :text}, [~D[2015-01-02]]) === "2015-01-02"
    assert apply_function({:cast, :text}, [Duration.from_days(3)]) === "P3D"
  end

  test "cast to boolean typing" do
    assert well_typed?({:cast, :boolean}, [:text])
    assert well_typed?({:cast, :boolean}, [:boolean])
    assert well_typed?({:cast, :boolean}, [:real])
    assert well_typed?({:cast, :boolean}, [:integer])
    refute well_typed?({:cast, :boolean}, [:datetime])
    refute well_typed?({:cast, :boolean}, [:date])
    refute well_typed?({:cast, :boolean}, [:time])
    refute well_typed?({:cast, :boolean}, [:interval])
  end

  test "cast to boolean" do
    assert apply_function({:cast, :boolean}, [1]) === true
    assert apply_function({:cast, :boolean}, [0]) === false
    assert apply_function({:cast, :boolean}, [123]) === true
    assert apply_function({:cast, :boolean}, [0.01]) === false
    assert apply_function({:cast, :boolean}, [0.9]) === true
    assert apply_function({:cast, :boolean}, ["tRuE"]) === true
    assert apply_function({:cast, :boolean}, ["fAlSe"]) === false
    assert apply_function({:cast, :boolean}, ["Bob"]) === nil
    assert apply_function({:cast, :boolean}, [true]) === true
    assert apply_function({:cast, :boolean}, [false]) === false
  end

  test "cast to datetime typing" do
    assert well_typed?({:cast, :datetime}, [:text])
    refute well_typed?({:cast, :datetime}, [:boolean])
    refute well_typed?({:cast, :datetime}, [:real])
    refute well_typed?({:cast, :datetime}, [:integer])
    assert well_typed?({:cast, :datetime}, [:datetime])
    refute well_typed?({:cast, :datetime}, [:date])
    refute well_typed?({:cast, :datetime}, [:time])
    refute well_typed?({:cast, :datetime}, [:interval])
  end

  test "cast to datetime" do
    time = ~N[2015-01-01 10:22:23.000000]
    assert apply_function({:cast, :datetime}, [time]) === time
    assert apply_function({:cast, :datetime}, [Timex.format!(time, "{ISO:Extended:Z}")]) === time
    assert apply_function({:cast, :datetime}, ["some string"]) === nil
  end

  test "cast to time typing" do
    assert well_typed?({:cast, :time}, [:text])
    refute well_typed?({:cast, :time}, [:boolean])
    refute well_typed?({:cast, :time}, [:real])
    refute well_typed?({:cast, :time}, [:integer])
    assert well_typed?({:cast, :time}, [:datetime])
    refute well_typed?({:cast, :time}, [:date])
    assert well_typed?({:cast, :time}, [:time])
    refute well_typed?({:cast, :time}, [:interval])
  end

  test "cast to time" do
    time = ~N[2015-01-02 03:04:05.000000]
    assert apply_function({:cast, :time}, [time]) === ~T[03:04:05.000000]
    assert apply_function({:cast, :time}, [~T[01:02:03.000000]]) === ~T[01:02:03.000000]
    assert apply_function({:cast, :time}, ["12:00:23"]) === ~T[12:00:23.000000]
    assert apply_function({:cast, :time}, ["some string"]) === nil
  end

  test "cast to date typing" do
    assert well_typed?({:cast, :date}, [:text])
    refute well_typed?({:cast, :date}, [:boolean])
    refute well_typed?({:cast, :date}, [:real])
    refute well_typed?({:cast, :date}, [:integer])
    assert well_typed?({:cast, :date}, [:datetime])
    assert well_typed?({:cast, :date}, [:date])
    refute well_typed?({:cast, :date}, [:time])
    refute well_typed?({:cast, :date}, [:interval])
  end

  test "cast to date" do
    time = ~N[2015-01-02 03:04:05]
    assert apply_function({:cast, :date}, [time]) === ~D[2015-01-02]
    assert apply_function({:cast, :date}, [~D[2015-01-01]]) === ~D[2015-01-01]
    assert apply_function({:cast, :date}, ["2016-01-02"]) === ~D[2016-01-02]
    assert apply_function({:cast, :date}, ["some string"]) === nil
  end

  test "cast to interval typing" do
    assert well_typed?({:cast, :interval}, [:text])
    refute well_typed?({:cast, :interval}, [:boolean])
    refute well_typed?({:cast, :interval}, [:real])
    refute well_typed?({:cast, :interval}, [:integer])
    refute well_typed?({:cast, :interval}, [:datetime])
    refute well_typed?({:cast, :interval}, [:date])
    refute well_typed?({:cast, :interval}, [:time])
    assert well_typed?({:cast, :interval}, [:interval])
  end

  test "cast to interval" do
    interval = Duration.from_seconds(10)
    assert apply_function({:cast, :interval}, [interval]) === interval
    assert apply_function({:cast, :interval}, ["PT10S"]) === interval
    assert apply_function({:cast, :interval}, ["not an interval"]) === nil
  end

  for type <- [:text, :boolean, :real, :integer, :datetime, :date, :time, :interval] do
    test "casting nil to #{type}" do
      assert apply_function({:cast, unquote(type)}, [nil]) === nil
    end
  end

  test "functions return nil when called with bad arguments" do
    assert apply_function("length", [nil]) == nil
    assert apply_function("+", [1, nil]) == nil
    assert apply_function("/", [1, 0]) == nil
  end

  test "compiling already extract_match function creates regex" do
    function = {:function, "extract_match", [%Column{}, %Column{value: "regex_pattern"}]}
    callback = fn(a) -> a end
    assert {:function, _, [_, %Column{value: %Regex{}}]} = Function.compile_function(function, callback)
  end

  test "compiling already compiled extract_match function does nothing" do
    function = {:function, "extract_match", [%Column{}, %Column{value: "regex_pattern"}]}
    callback = fn(a) -> a end
    compiled_function = Function.compile_function(function, callback)
    assert Function.compile_function(compiled_function, callback) == compiled_function
  end

  test "compiling function that doesn't need compilation does nothing" do
    function = {:function, "no_compilation", [:args1, :args2]}
    callback = fn(a) -> a end
    assert function == Function.compile_function(function, callback)
  end


  defp return_type(name, arg_types), do:
    Function.return_type({:function, name, Enum.map(arg_types, &Column.constant(&1, nil))})

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
