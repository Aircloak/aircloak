defmodule Cloak.Sql.Function.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Expression, Function}

  for function <- ~w(floor ceil ceiling) do
    test "#{function} argument types" do
      assert well_typed?(unquote(function), [:integer])
      assert well_typed?(unquote(function), [:real])
    end
  end

  test "round" do
    assert well_typed?("round", [:real])
    assert well_typed?("round", [:integer])
  end

  test "binary round" do
    assert well_typed?("round", [:real, :integer])
    assert well_typed?("round", [:integer, :integer])
  end

  test "trunc" do
    assert well_typed?("round", [:real])
    assert well_typed?("round", [:integer])
  end

  test "binary trunc" do
    assert well_typed?("round", [:real, :integer])
    assert well_typed?("round", [:integer, :integer])
  end

  test "length", do:
    assert well_typed?("length", [:text])

  test "left", do:
    assert well_typed?("left", [:text, :integer])

  test "right", do:
    assert well_typed?("right", [:text, :integer])

  test "lower" do
    assert well_typed?("lower", [:text])
    assert well_typed?("lcase", [:text])
  end

  test "upper" do
    assert well_typed?("upper", [:text])
    assert well_typed?("ucase", [:text])
  end

  test "btrim" do
    assert well_typed?("btrim", [:text])
    assert well_typed?("btrim", [:text, :text])
  end

  test "ltrim" do
    assert well_typed?("ltrim", [:text])
    assert well_typed?("ltrim", [:text, :text])
  end

  test "rtrim" do
    assert well_typed?("rtrim", [:text])
    assert well_typed?("rtrim", [:text, :text])
  end

  test "substring" do
    assert well_typed?("substring", [:text, :integer])
    assert well_typed?("substring", [:text, :integer, :integer])
  end

  test "concat" do
    assert well_typed?("concat", [:text])
    assert well_typed?("concat", [:text, :text])
    assert well_typed?("concat", [:text, :text, :text, :text, :text])
    refute well_typed?("concat", [:text, :text, :integer, :text, :text])
  end

  for function <- ~w(year quarter month day weekday) do
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

  test "date_trunc typing" do
    assert return_type("date_trunc", [:text, :datetime]) == :datetime
    assert return_type("date_trunc", [:text, :time]) == :time
    assert return_type("date_trunc", [:text, :date]) == :datetime
    refute well_typed?("date_trunc", [:date, :date])
    refute well_typed?("date_trunc", [:text, :integer])
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
  end

  test "subtracting times" do
    assert well_typed?("-", [:time, :time])
    assert return_type("-", [:time, :time]) == :interval
  end

  test "subtracting datetimes" do
    assert well_typed?("-", [:datetime, :datetime])
    assert return_type("-", [:datetime, :datetime]) == :interval
  end

  test "date + interval" do
    assert well_typed?("+", [:date, :interval])
    assert return_type("+", [:date, :interval]) == :datetime
  end

  test "time + interval" do
    assert well_typed?("+", [:time, :interval])
    assert return_type("+", [:time, :interval]) == :time
  end

  test "datetime + interval" do
    assert well_typed?("+", [:datetime, :interval])
    assert return_type("+", [:datetime, :interval]) == :datetime
  end

  for {type, _value} <- %{time: ~T[10:20:30], date: ~D[2015-01-02], datetime: ~N[2015-01-02 10:20:30]} do
    test "#{type} - interval", do:
      assert well_typed?("-", [unquote(type), :interval])

    test "interval - #{type} is ill-typed", do:
      refute well_typed?("-", [:interval, unquote(type)])

    test "interval + #{type}", do:
      assert well_typed?("+", [:interval, unquote(type)])
  end

  test "interval + interval" do
    assert well_typed?("+", [:interval, :interval])
    assert return_type("+", [:interval, :interval]) == :interval
  end

  test "interval - interval" do
    assert well_typed?("-", [:interval, :interval])
    assert return_type("-", [:interval, :interval]) == :interval
  end

  test "interval * number" do
    assert return_type("*", [:interval, :integer]) == :interval
    assert return_type("*", [:integer, :interval]) == :interval
    assert return_type("*", [:interval, :real]) == :interval
    assert return_type("*", [:real, :interval]) == :interval
  end

  test "interval / number" do
    assert return_type("/", [:interval, :integer]) == :interval
    assert return_type("/", [:interval, :real]) == :interval
  end

  test "number / interval is ill-typed" do
    refute well_typed?("/", [:integer, :interval])
    refute well_typed?("/", [:real, :interval])
  end

  test "% typing" do
    assert well_typed?("%", [:integer, :integer])
    refute well_typed?("%", [:real, :real])
  end

  test "typechecking a nested function call" do
    assert Function.well_typed?({:function, "avg", [{:function, "abs", [Expression.constant(:integer, 3)]}]})
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

  test "can tell when a function splits rows", do:
    assert Function.has_attribute?("extract_words", :row_splitter)

  test "can tell when a function does not split rows", do:
    refute Function.has_attribute?("substring", :row_splitter)

  test "knows `ceil` is allowed in a subquery", do:
    refute Function.has_attribute?({:function, "ceil", []}, :not_in_subquery)

  test "returns true if function exists", do: assert Function.exists?({:function, "*", []})

  test "returns false if function does not exists", do: refute Function.exists?({:function, "foobar", []})

  defp return_type(name, arg_types), do:
    Function.return_type({:function, name, Enum.map(arg_types, &Expression.constant(&1, nil))})

  defp well_typed?(name, types), do:
    Function.well_typed?({:function, name, Enum.map(types, &Expression.constant(&1, nil))})
end
