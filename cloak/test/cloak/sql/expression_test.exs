defmodule Cloak.Sql.Expression.Test do
  require Integer
  use ExUnit.Case, async: true

  alias Cloak.Sql.Expression
  alias Timex.Duration

  test "coalesce" do
    assert nil == apply_function("coalesce", [nil, nil])
    assert 3 = apply_function("coalesce", [nil, 3, nil])
  end

  test "sqrt" do
    assert_in_delta(apply_function("sqrt", [3]), 1.73, 0.1)
    assert apply_function("sqrt", [nil]) === nil
  end

  test "floor" do
    assert apply_function("floor", [3.99]) === 3
    assert apply_function("floor", [3.01]) === 3
    assert apply_function("floor", [-3.99]) === -4
    assert apply_function("floor", [3]) === 3
    assert apply_function("floor", [pow(10, 5000)]) === pow(10, 5000)
    assert apply_function("floor", [nil]) === nil
  end

  test "ceil" do
    assert apply_function("ceil", [3.99]) === 4
    assert apply_function("ceil", [3.01]) === 4
    assert apply_function("ceil", [3]) === 3
    assert apply_function("ceil", [pow(10, 5000)]) === pow(10, 5000)
    assert apply_function("ceil", [nil]) === nil
  end

  test "abs" do
    assert apply_function("abs", [1.2]) == 1.2
    assert apply_function("abs", [-1.2]) == 1.2
    assert apply_function("abs", [1]) == 1
    assert apply_function("abs", [-1]) == 1
    assert apply_function("abs", [nil]) == nil
  end

  test "round" do
    assert apply_function("round", [3.99]) == 4
    assert apply_function("round", [3.01]) == 3
    assert apply_function("round", [3]) == 3
    assert apply_function("round", [pow(10, 5000)]) === pow(10, 5000)
    assert apply_function("round", [nil]) == nil
  end

  test "binary round" do
    assert apply_function("round", [3.99, 1]) == 4.0
    assert apply_function("round", [3.91, 1]) == 3.9
    assert apply_function("round", [3.991, 2]) == 3.99
    assert apply_function("round", [3.99, 4]) == 3.99
    assert apply_function("round", [3, 1]) == 3
    assert apply_function("round", [pow(10, 5000), 1]) === pow(10, 5000)
    assert apply_function("round", [nil, 1]) == nil
    assert apply_function("round", [3.5, nil]) == nil
    assert apply_function("round", [33, -1]) == 30
    assert apply_function("round", [36, -1]) == 40
    assert apply_function("round", [333, -2]) == 300
  end

  test "trunc" do
    assert apply_function("trunc", [3.99]) == 3
    assert apply_function("trunc", [-3.99]) == -3
    assert apply_function("trunc", [3]) == 3
    assert apply_function("trunc", [pow(10, 5000)]) === pow(10, 5000)
    assert apply_function("trunc", [nil]) == nil
  end

  test "binary trunc" do
    assert apply_function("trunc", [3.99, 1]) == 3.9
    assert apply_function("trunc", [-3.99, 1]) == -3.9
    assert apply_function("trunc", [3.99, 2]) == 3.99
    assert apply_function("trunc", [3.99, 4]) == 3.99
    assert apply_function("trunc", [3, 4]) == 3
    assert apply_function("trunc", [pow(10, 5000), 1]) === pow(10, 5000)
    assert apply_function("trunc", [nil, 4]) == nil
    assert apply_function("trunc", [39, -1]) == 30
    assert apply_function("trunc", [399, -2]) == 300
    assert apply_function("trunc", [31, -1]) == 30
  end

  test "div" do
    assert apply_function("div", [12, 3]) == 4
    assert apply_function("div", [13, 3]) == 4
    assert apply_function("div", [nil, 3]) == nil
    assert apply_function("div", [2, nil]) == nil
  end

  test "%" do
    assert apply_function("%", [13, 3]) == 1
    assert apply_function("%", [nil, 3]) == nil
    assert apply_function("%", [13, nil]) == nil
  end

  test "^" do
    assert apply_function("^", [2, 3]) == 8
    assert apply_function("^", [2, nil]) == nil
    assert apply_function("^", [nil, 3]) == nil
  end

  test "length" do
    assert apply_function("length", ["a string"]) == 8
    assert apply_function("length", [nil]) == nil
  end

  test "left" do
    assert apply_function("left", ["a string", 2]) == "a "
    assert apply_function("left", ["a string", -2]) == "a stri"
    assert apply_function("left", ["a string", -10]) == ""
    assert apply_function("left", [nil, 2]) == nil
    assert apply_function("left", ["a string", nil]) == nil
  end

  test "right" do
    assert apply_function("right", ["a string", 2]) == "ng"
    assert apply_function("right", ["a string", -2]) == "string"
    assert apply_function("right", [nil, 0]) == nil
    assert apply_function("right", ["a string", nil]) == nil
    assert apply_function("right", ["a string", 100]) == "a string"
  end

  test "lower" do
    assert apply_function("lower", ["A sTrinG"]) == "a string"
    assert apply_function("lower", [nil]) == nil
  end

  test "upper" do
    assert apply_function("upper", ["A sTrinG"]) == "A STRING"
    assert apply_function("upper", [nil]) == nil
  end

  test "btrim" do
    assert apply_function("btrim", ["  a string "]) == "a string"
    assert apply_function("btrim", ["xyxa stringxyx", "xy"]) == "a string"
    assert apply_function("btrim", [nil]) == nil
  end

  test "ltrim" do
    assert apply_function("ltrim", ["  a string "]) == "a string "
    assert apply_function("ltrim", ["xyxa stringxyx", "xy"]) == "a stringxyx"
    assert apply_function("ltrim", [nil]) == nil
  end

  test "rtrim" do
    assert apply_function("rtrim", ["  a string "]) == "  a string"
    assert apply_function("rtrim", [nil]) == nil
    assert apply_function("rtrim", ["xyxa stringxyx", "xy"]) == "xyxa string"
  end

  test "quarter" do
    [
      {"01", 1},
      {"02", 1},
      {"03", 1},
      {"04", 2},
      {"05", 2},
      {"06", 2},
      {"07", 3},
      {"08", 3},
      {"09", 3},
      {"10", 4},
      {"11", 4},
      {"12", 4}
    ]
    |> Enum.each(fn {month, quarter} ->
      date = Date.from_iso8601!("2017-#{month}-01")
      assert apply_function("quarter", [date]) == quarter
    end)
  end

  test "substring" do
    assert apply_function("substring", ["a string", 3]) == "string"
    assert apply_function("substring", ["a string", 3, 2]) == "st"
    assert apply_function("substring", [nil, 1, 1]) == nil
    assert apply_function("substring", ["  ", nil]) == nil
  end

  test "concat", do: assert(apply_function("concat", ["a", " ", "string"]) == "a string")

  test "subtracting dates", do: assert(apply_function("-", [~D[2015-01-30], ~D[2015-01-20]]) === Duration.from_days(10))

  test "subtracting times", do: assert(apply_function("-", [~T[10:20:00], ~T[10:00:00]]) === Duration.from_minutes(20))

  test "subtracting datetimes",
    do: assert(apply_function("-", [~N[2015-01-02 10:20:00], ~N[2015-01-01 10:00:00]]) === Duration.parse!("P1DT20M"))

  test "date + interval",
    do: assert(apply_function("+", [~D[2015-01-01], Duration.parse!("P10DT10M")]) === ~N[2015-01-11 00:10:00])

  test "time + interval",
    do: assert(apply_function("+", [~T[10:20:30], Duration.parse!("P10DT1H10M")]) === ~T[11:30:30])

  test "datetime + interval",
    do: assert(apply_function("+", [~N[2015-01-01 10:20:30], Duration.parse!("P10DT10M")]) === ~N[2015-01-11 10:30:30])

  for {type, value} <- %{
        time: ~T[10:20:30],
        date: ~D[2015-01-02],
        datetime: ~N[2015-01-02 10:20:30]
      } do
    @value value
    @interval Duration.parse!("P10DT10M")

    test "#{type} - interval",
      do:
        assert(
          apply_function("-", [@value, @interval]) === apply_function("+", [@value, Duration.scale(@interval, -1)])
        )

    test "interval + #{type}",
      do: assert(apply_function("+", [@interval, @value]) == apply_function("+", [@value, @interval]))
  end

  test "interval + interval",
    do: assert(apply_function("+", [Duration.parse!("P10D"), Duration.parse!("PT10M")]) === Duration.parse!("P10DT10M"))

  test "interval - interval",
    do:
      assert(
        apply_function("-", [Duration.parse!("P10DT10M"), Duration.parse!("P1DT1M")]) === Duration.parse!("P9DT9M")
      )

  test "interval * number" do
    assert apply_function("*", [Duration.parse!("P1DT1M"), 10]) === Duration.parse!("P10DT10M")
    assert apply_function("*", [10, Duration.parse!("P1DT1M")]) === Duration.parse!("P10DT10M")
    assert apply_function("*", [Duration.parse!("P10DT10M"), 0.5]) === Duration.parse!("P5DT5M")
  end

  test "interval / number" do
    assert apply_function("/", [Duration.parse!("P10DT10M"), 2]) === Duration.parse!("P5DT5M")
    assert apply_function("/", [Duration.parse!("P10DT10M"), 0.5]) === Duration.parse!("P20DT20M")
  end

  test "any function with one of the arguments being :*", do: assert(apply_function("whatever", [1, :*, "thing"]) == :*)

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

  test "cast to datetime" do
    time = ~N[2015-01-01 10:22:23.000000]
    assert apply_function({:cast, :datetime}, [time]) === time
    assert apply_function({:cast, :datetime}, [Timex.format!(time, "{ISO:Extended:Z}")]) === time
    assert apply_function({:cast, :datetime}, ["some string"]) === nil
  end

  test "cast to time" do
    time = ~N[2015-01-02 03:04:05.000000]
    assert apply_function({:cast, :time}, [time]) === ~T[03:04:05.000000]
    assert apply_function({:cast, :time}, [~T[01:02:03.000000]]) === ~T[01:02:03.000000]
    assert apply_function({:cast, :time}, ["12:00:23"]) === ~T[12:00:23.000000]
    assert apply_function({:cast, :time}, ["some string"]) === nil
  end

  test "cast to date" do
    time = ~N[2015-01-02 03:04:05]
    assert apply_function({:cast, :date}, [time]) === ~D[2015-01-02]
    assert apply_function({:cast, :date}, [~D[2015-01-01]]) === ~D[2015-01-01]
    assert apply_function({:cast, :date}, ["2016-01-02"]) === ~D[2016-01-02]
    assert apply_function({:cast, :date}, ["some string"]) === nil
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

  test "math functions return nil when called with bad arguments" do
    assert apply_function("+", [1, nil]) == nil
    assert apply_function("*", [nil, 1]) == nil
    assert apply_function("/", [1, 0]) == nil
    assert apply_function("sqrt", [-1.0]) === nil
    assert apply_function("div", [12, 0]) == nil
  end

  test "extract_words" do
    assert apply_function("extract_words", ["some words"]) == ["some", "words"]
    assert apply_function("extract_words", [nil]) == [nil]
  end

  describe "first_column" do
    test "nil if given constant column", do: assert(nil == Expression.first_column(%Expression{constant?: true}))

    test "first db column if one present" do
      return_column = %Expression{row_index: 1}

      assert return_column ==
               Expression.first_column(
                 Expression.function("f", [
                   Expression.function("f", [%Expression{constant?: true}]),
                   Expression.function("f", [%Expression{constant?: true}, return_column])
                 ])
               )
    end
  end

  describe "lowercase" do
    test "wraps expressions in lower case functions",
      do: assert(%Expression{function: "lower"} = Expression.lowercase(%Expression{type: :text}))

    test "makes constant text lowercase",
      do: assert(%Expression{value: "case"} = Expression.lowercase(Expression.constant(:text, "CaSe")))

    test "lowercases like patterns",
      do:
        assert(Expression.like_pattern("a%b_c", nil) == Expression.like_pattern("A%b_C", nil) |> Expression.lowercase())

    test "fails if not a textual expression",
      do:
        assert_raise(RuntimeError, fn ->
          Expression.lowercase(Expression.constant(:integer, 1))
        end)
  end

  defp apply_function(name, args) do
    name
    |> Expression.function(Enum.map(args, &Expression.constant(nil, &1)))
    |> Expression.value([])
  end

  defp pow(_, 0), do: 1
  defp pow(x, n) when Integer.is_odd(n), do: x * pow(x, n - 1)

  defp pow(x, n) do
    result = pow(x, div(n, 2))
    result * result
  end

  test "expression alias validation" do
    refute Expression.valid_alias?("")
    refute Expression.valid_alias?("-")
    refute Expression.valid_alias?("_")
    refute Expression.valid_alias?(".")
    refute Expression.valid_alias?("asda..dsd")
    refute Expression.valid_alias?("asdad.")
    refute Expression.valid_alias?("21a")
    refute Expression.valid_alias?("#_")
    refute Expression.valid_alias?("_2")

    assert Expression.valid_alias?("_as")
    assert Expression.valid_alias?("aa33.d")
    assert Expression.valid_alias?("a_22")
  end

  describe "expression display" do
    test "count(*)" do
      assert Expression.function("count", [:*]) |> Expression.display() == "count(*)"
    end

    test "col * 3.4" do
      column = Expression.column(%{name: "col", type: :integer}, %{name: "table", user_id: "uid"})
      constant = Expression.constant(:real, 3.4)

      assert Expression.function("*", [column, constant]) |> Expression.display() == "col * 3.4"
    end

    test "sum(distinct abs(col))" do
      column = Expression.column(%{name: "col", type: :integer}, %{name: "table", user_id: "uid"})

      assert Expression.function("sum", [{:distinct, Expression.function("abs", [column])}])
             |> Expression.display() == "sum(distinct abs(col))"
    end

    test "cast('123' as integer)" do
      constant = Expression.constant(:text, "123")

      assert Expression.function({:cast, :integer}, [constant]) |> Expression.display() == "cast('123' as integer)"
    end

    test "interval" do
      assert Expression.constant(:interval, Timex.Duration.parse!("PT1H1M1S")) |> Expression.display() ==
               "interval 'PT1H1M1S'"
    end

    test "date" do
      assert Expression.constant(:date, ~D[2016-02-01]) |> Expression.display() == "date '2016-02-01'"
    end
  end
end
