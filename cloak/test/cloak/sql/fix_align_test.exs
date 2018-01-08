defmodule Cloak.Sql.FixAlign.Test do
  use ExUnit.Case, async: true
  use ExCheck

  alias Cloak.Sql.FixAlign

  for interval_type <- [:int, :float] do
    property "aligned #{interval_type} interval contains both ends of input" do
      for_all {x, y} in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align_interval({x, y})
        left <= x && y <= right
      end
    end

    property "the endpoints of an #{interval_type} interval are multiples of half the size" do
      for_all interval in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align_interval(interval)
        half_size = (right - left) / 2
        even_multiple?(left, half_size) && even_multiple?(right, half_size)
      end
    end

    property "the size of an aligned #{interval_type} interval is money-aligned" do
      for_all interval in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align_interval(interval)
        size = right - left
        even_power_of_10?(size) || even_power_of_10?(size / 2) || even_power_of_10?(size / 5)
      end
    end
  end

  for interval_type <- [:int, :datetime, :date, :time] do
    property "align_interval is idempotent on #{interval_type} intervals" do
      for_all interval in interval(unquote(interval_type)) do
        interval |> FixAlign.align_interval() == interval |> FixAlign.align_interval() |> FixAlign.align_interval()
      end
    end
  end

  property "an aligned interval is not much larger than the input" do
    for_all {x, y} in float_interval() do
      interval = {x / 10, y / 10}
      10 * width(interval) >= interval |> FixAlign.align_interval() |> width()
    end
  end

  for interval_type <- [:datetime, :date] do
    property "aligned #{interval_type} interval contains both ends of the input" do
      for_all {x, y} in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align_interval({x, y})
        Timex.diff(x, left) >= 0 && Timex.diff(right, y) >= 0
      end
    end

    property "an aligned #{interval_type} interval is not much larger than the input" do
      for_all {x, y} in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align_interval({x, y})

        if Timex.diff(y, x, :minutes) < 1 do
          Timex.diff(right, left) <= 10 * Timex.diff(y, x)
        else
          Timex.diff(right, left) <= 6.1 * Timex.diff(y, x)
        end
      end
    end
  end

  property "an aligned time interval contains both ends of the input" do
    for_all {x, y} in time_interval() do
      {left, right} = FixAlign.align_interval({x, y})
      lt_eq(left, x) && lt_eq(y, right)
    end
  end

  test "align datetime interval" do
    assert FixAlign.align_interval({~N[2010-01-01 00:00:00.000000], ~N[2012-10-01 00:00:00.000000]}) ==
      {~N[2010-01-01 00:00:00.000000], ~N[2015-01-01 00:00:00.000000]}
    assert FixAlign.align_interval({~D[2010-01-01], ~D[2012-10-01]}) == {~D[2010-01-01], ~D[2015-01-01]}
    assert FixAlign.align_interval({~N[2010-12-30 00:00:00.000000], ~N[2011-01-30 00:00:00.000000]}) ==
      {~N[2010-12-01 00:00:00.000000], ~N[2011-02-01 00:00:00.000000]}
    assert FixAlign.align_interval({~N[2010-10-10 00:00:00.000000], ~N[2010-10-19 00:00:00.000000]}) ==
      {~N[2010-10-08 00:00:00.000000], ~N[2010-10-28 00:00:00.000000]}
    assert FixAlign.align_interval({~N[2000-05-31 11:19:05.000000], ~N[2000-07-07 11:46:44.000000]}) ==
      FixAlign.align_interval({~N[2000-05-30 11:19:05.000000], ~N[2000-07-07 11:46:44.000000]})
  end

  test "aligning date intervals" do
    assert FixAlign.align_interval({~D[1997-08-16], ~D[1997-09-16]}) == {~D[1997-08-01], ~D[1997-10-01]}
  end

  test "aligning dates doesn't consider half-days" do
    assert Cloak.Sql.FixAlign.align_interval({~D[2000-06-10], ~D[2000-06-13]}) == {~D[2000-06-07], ~D[2000-06-17]}
  end

  test "aligning intervals before epoch" do
    assert Cloak.Sql.FixAlign.align_interval({~D[1956-11-25], ~D[1957-11-04]}) == {~D[1956-01-01], ~D[1958-01-01]}
    assert Cloak.Sql.FixAlign.align_interval({~D[1959-09-14], ~D[1963-12-14]}) == {~D[1955-01-01], ~D[1965-01-01]}
  end

  test "1583-01-01 is the minimum date" do
    assert {~D[1583-01-01], _} = Cloak.Sql.FixAlign.align_interval({~D[1000-01-01], ~D[3000-01-01]})
    assert {~D[1583-01-01], _} = Cloak.Sql.FixAlign.align_interval({~D[1583-01-01], ~D[1583-01-12]})
    assert {~N[1583-01-01 00:00:00.000000], _} =
      Cloak.Sql.FixAlign.align_interval({~N[1000-01-01 00:00:00], ~N[3000-01-01 00:00:00]})
    assert {~N[1583-01-01 00:00:00.000000], _} =
      Cloak.Sql.FixAlign.align_interval({~N[1583-01-01 00:00:00], ~N[1583-01-12 00:00:00]})
  end

  test "9999-12-31 is the maximum date" do
    assert {_, ~D[9999-12-31]} = Cloak.Sql.FixAlign.align_interval({~D[2000-01-01], ~D[9950-01-01]})
    assert {_, ~D[9999-12-31]} = Cloak.Sql.FixAlign.align_interval({~D[9999-12-20], ~D[9999-12-31]})
    assert {_, ~N[9999-12-31 23:59:59.999999]} =
      Cloak.Sql.FixAlign.align_interval({~N[2000-01-01 00:00:00], ~N[9950-01-01 00:00:00]})
    assert {_, ~N[9999-12-31 23:59:59.999999]} =
      Cloak.Sql.FixAlign.align_interval({~N[9999-12-31 20:00:00], ~N[9999-12-31 23:00:00]})
  end

  test "align time intervals" do
    assert FixAlign.align_interval({~T[10:20:30], ~T[10:20:34]}) == {~T[10:20:30.000000], ~T[10:20:35.000000]}
    assert FixAlign.align_interval({~T[10:23:30], ~T[10:30:00]}) == {~T[10:22:30.000000], ~T[10:37:30.000000]}
    assert FixAlign.align_interval({~T[09:20:30], ~T[10:20:34]}) == {~T[09:00:00.000000], ~T[11:00:00.000000]}
  end

  property "numbers are money-aligned" do
    for_all x in (such_that y in float() when y != 0) do
      result = x |> FixAlign.align() |> abs()
      even_power_of_10?(result) || even_power_of_10?(result / 2) || even_power_of_10?(result / 5)
    end
  end

  property "money-aligned numbers are close to the input" do
    for_all x in (such_that y in float() when y != 0) do
      result = x |> FixAlign.align()
      abs(result - x) <= 0.45 * abs(x)
    end
  end

  property "align is idempotent" do
    for_all x in float() do
      x |> FixAlign.align() == x |> FixAlign.align() |> FixAlign.align()
    end
  end

  test "rounds the mid-point up" do
    assert FixAlign.align(0.75) == 1
    assert FixAlign.align(35) == 50
    assert FixAlign.align(15) == 20
  end

  defp interval(:int), do: int_interval()
  defp interval(:float), do: float_interval()
  defp interval(:datetime), do: datetime_interval()
  defp interval(:date), do: date_interval()
  defp interval(:time), do: time_interval()

  defp int_interval, do: such_that({x, y} in {int(), int()} when x < y)

  defp float_interval, do: such_that({x, y} in {float(), float()} when x < y)

  defp datetime_interval, do: such_that({x, y} in {datetime(), datetime()} when Timex.diff(x, y) < 0)

  defp date_interval, do: such_that({x, y} in {date(), date()} when Timex.diff(x, y) < 0)

  defp time_interval, do: such_that({x, y} in {time(), time()} when lt_eq(x, y) and x != y)

  defp datetime do
    domain(
      :datetime,
      _generate = fn(domain, size) ->
        size = size |> :math.pow(4) |> round()
        {domain, Timex.shift(~N[2000-06-15 12:20:30], seconds: draw(int(), size))}
      end,
      _shrink = fn(domain, item) -> {domain, item} end
    )
  end

  defp date do
    domain(
      :datetime,
      _generate = fn(domain, size) ->
        size = size |> :math.pow(2) |> round()
        {domain, Timex.shift(~D[2000-06-15], days: draw(int(), size))}
      end,
      _shrink = fn(domain, item) -> {domain, item} end
    )
  end

  @seconds_in_day 24 * 60 * 60
  defp time do
    domain(
      :time,
      _generate = fn(domain, size) ->
        size = size |> :math.pow(2.3) |> round() |> min(@seconds_in_day)
        {domain, draw(pos_integer(), size) |> min(@seconds_in_day - 1) |> Cloak.Time.from_integer(:time)}
      end,
      _shrink = fn(domain, item) -> {domain, item} end
    )
  end

  defp draw(domain, size) do
    {_, result} = pick(domain, size)
    result
  end

  defp even_power_of_10?(x) do
    log = :math.log10(x)
    abs(round(log) - log) < epsilon()
  end

  defp even_multiple?(x, y) do
    abs(round(x / y) - (x / y)) <= abs((x / y) * epsilon())
  end

  defp epsilon, do: 1.0e-6

  defp width({x, y}), do: y - x

  defp lt_eq(%Time{} = x, %Time{} = y), do: Cloak.Time.to_integer(x) <= Cloak.Time.to_integer(y)
end
