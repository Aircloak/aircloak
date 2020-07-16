defmodule Cloak.Sql.FixAlign.Test do
  use ExUnit.Case, async: true
  use ExUnitProperties
  alias Cloak.Sql.FixAlign

  test "aligning a boolean interval", do: assert({false, true} = FixAlign.align_interval({false, true}))

  for interval_type <- [:int, :float] do
    property "aligned #{interval_type} interval contains both ends of input" do
      check all({x, y} <- interval(unquote(interval_type))) do
        {left, right} = FixAlign.align_interval({x, y})
        assert weak_lt_eq(left, x)
        assert weak_lt_eq(y, right)
      end
    end

    property "the endpoints of an #{interval_type} interval are multiples of half the size" do
      check all(interval <- interval(unquote(interval_type))) do
        {left, right} = FixAlign.align_interval(interval)
        half_size = (right - left) / 2
        assert even_multiple?(left, half_size)
        assert even_multiple?(right, half_size)
      end
    end

    property "the size of an aligned #{interval_type} interval is money-aligned" do
      check all(interval <- interval(unquote(interval_type))) do
        {left, right} = FixAlign.align_interval(interval)
        size = right - left
        assert even_power_of_10?(size) || even_power_of_10?(size / 2) || even_power_of_10?(size / 5)
      end
    end

    property "small change in input results in either no change or a big change in output for #{interval_type}" do
      check all(
              {left, right} <- interval(unquote(interval_type)),
              delta_left <- float(min: -0.01, max: 0.01),
              delta_right <- float(min: -0.01, max: 0.01)
            ) do
        {output1_left, output1_right} = FixAlign.align_interval({left, right})
        perturbed = [left * (1 + delta_left), right * (1 + delta_right)] |> Enum.sort() |> List.to_tuple()
        {output2_left, output2_right} = FixAlign.align_interval(perturbed)

        assert {output1_left, output1_right} == {output2_left, output2_right} or
                 abs(output1_left - output2_left) > 0.3 * output1_left or
                 abs(output1_right - output2_right) > 0.3 * output1_right
      end
    end
  end

  for interval_type <- [:int, :datetime, :date, :time] do
    property "align_interval is idempotent on #{interval_type} intervals" do
      check all(interval <- interval(unquote(interval_type))) do
        assert interval |> FixAlign.align_interval() ==
                 interval |> FixAlign.align_interval() |> FixAlign.align_interval()
      end
    end
  end

  property "an aligned interval is not much larger than the input" do
    check all({x, y} <- float_interval()) do
      interval = {x / 10, y / 10}
      assert 10 * width(interval) >= interval |> FixAlign.align_interval() |> width()
    end
  end

  for interval_type <- [:datetime, :date] do
    property "aligned #{interval_type} interval contains both ends of the input" do
      check all({x, y} <- interval(unquote(interval_type)), legal_datetime?(x) and legal_datetime?(y)) do
        {left, right} = FixAlign.align_interval({x, y})
        assert Timex.diff(x, left) >= 0 && Timex.diff(right, y) >= 0
      end
    end

    property "an aligned #{interval_type} interval is not much larger than the input" do
      check all({x, y} <- interval(unquote(interval_type))) do
        {left, right} = FixAlign.align_interval({x, y})

        if Timex.diff(y, x, :minutes) < 1 do
          assert Timex.diff(right, left) <= 10 * Timex.diff(y, x)
        else
          assert Timex.diff(right, left) <= 6.1 * Timex.diff(y, x)
        end
      end
    end
  end

  property "an aligned time interval contains both ends of the input" do
    check all({x, y} <- time_interval()) do
      {left, right} = FixAlign.align_interval({x, y})
      assert lt_eq(left, x) && lt_eq(y, right)
    end
  end

  test "align datetime interval" do
    assert FixAlign.align_interval({~N[2010-01-01 00:00:00.000000], ~N[2012-10-01 00:00:00.000000]}) ==
             {~N[2010-01-01 00:00:00.000000], ~N[2014-01-01 00:00:00.000000]}

    assert FixAlign.align_interval({~D[2010-01-01], ~D[2012-10-01]}) == {~D[2010-01-01], ~D[2014-01-01]}

    assert FixAlign.align_interval({~N[2010-12-30 00:00:00.000000], ~N[2011-01-30 00:00:00.000000]}) ==
             {~N[2010-12-01 00:00:00.000000], ~N[2011-02-01 00:00:00.000000]}

    assert FixAlign.align_interval({~N[2010-10-10 00:00:00.000000], ~N[2010-10-19 00:00:00.000000]}) ==
             {~N[2010-10-01 00:00:00.000000], ~N[2010-10-21 00:00:00.000000]}

    assert FixAlign.align_interval({~N[2000-05-31 11:19:05.000000], ~N[2000-07-07 11:46:44.000000]}) ==
             FixAlign.align_interval({~N[2000-05-30 11:19:05.000000], ~N[2000-07-07 11:46:44.000000]})
  end

  test "aligning date intervals" do
    assert FixAlign.align_interval({~D[1997-08-16], ~D[1997-09-16]}) == {~D[1997-08-01], ~D[1997-10-01]}
  end

  test "aligning dates doesn't consider half-days" do
    assert Cloak.Sql.FixAlign.align_interval({~D[2000-06-10], ~D[2000-06-13]}) == {~D[2000-06-10], ~D[2000-06-15]}
  end

  test "aligning intervals before epoch" do
    assert Cloak.Sql.FixAlign.align_interval({~D[1956-11-25], ~D[1957-11-04]}) == {~D[1956-01-01], ~D[1958-01-01]}

    assert Cloak.Sql.FixAlign.align_interval({~D[1959-09-14], ~D[1963-12-14]}) == {~D[1959-07-01], ~D[1966-07-01]}
  end

  test "1900-01-01 is the minimum date" do
    assert {~D[1900-01-01], _} = Cloak.Sql.FixAlign.align_interval({~D[1901-12-01], ~D[2950-01-01]})

    assert {~D[1900-01-01], _} = Cloak.Sql.FixAlign.align_interval({~D[1900-01-01], ~D[1900-01-12]})

    assert {~N[1900-01-01 00:00:00.000000], _} =
             Cloak.Sql.FixAlign.align_interval({~N[1900-01-01 00:00:00], ~N[9050-12-20 12:00:00]})

    assert {~N[1900-01-01 00:00:00.000000], _} =
             Cloak.Sql.FixAlign.align_interval({~N[1900-01-01 01:00:00], ~N[1900-01-12 00:30:00]})
  end

  test "9999-12-31 is the maximum date" do
    assert {_, ~D[9999-12-31]} = Cloak.Sql.FixAlign.align_interval({~D[2001-01-01], ~D[9950-01-01]})

    assert {_, ~D[9999-12-31]} = Cloak.Sql.FixAlign.align_interval({~D[9999-12-20], ~D[9999-12-31]})

    assert {_, ~N[9999-12-31 23:59:59.999999]} =
             Cloak.Sql.FixAlign.align_interval({~N[2020-01-01 00:00:00], ~N[9950-01-01 00:00:00]})

    assert {_, ~N[9999-12-31 23:59:59.999999]} =
             Cloak.Sql.FixAlign.align_interval({~N[9999-12-31 20:00:00], ~N[9999-12-31 23:00:00]})
  end

  test "align time intervals" do
    assert FixAlign.align_interval({~T[10:20:30], ~T[10:20:34]}) == {~T[10:20:30.000000], ~T[10:20:35.000000]}

    assert FixAlign.align_interval({~T[10:23:30], ~T[10:30:00]}) == {~T[10:22:30.000000], ~T[10:37:30.000000]}

    assert FixAlign.align_interval({~T[09:20:30], ~T[10:20:34]}) == {~T[09:00:00.000000], ~T[11:00:00.000000]}
  end

  test "[Issue #2344] months have a 1-2-3-6-9 grid",
    do: assert(FixAlign.align_interval({~D[2016-11-01], ~D[2017-07-01]}) == {~D[2016-09-01], ~D[2017-07-01]})

  test "[Bug] aligning {-74.025, -73.975}" do
    assert {a, b} = FixAlign.align_interval({-74.025, -73.975})
    assert_in_delta(a, -74.025, 0.00001)
    assert_in_delta(b, -73.975, 0.00001)
  end

  test "[Bug] aligning {73.975, 74.025}" do
    assert {a, b} = FixAlign.align_interval({73.975, 74.025})
    assert_in_delta(a, 73.975, 0.00001)
    assert_in_delta(b, 74.025, 0.00001)
  end

  property "numbers are money-aligned" do
    check all(x <- float(), x != 0) do
      result = x |> FixAlign.align() |> abs()
      assert even_power_of_10?(result) || even_power_of_10?(result / 2) || even_power_of_10?(result / 5)
    end
  end

  property "money-aligned numbers are close to the input" do
    check all(x <- float(), x != 0) do
      result = x |> FixAlign.align()
      assert abs(result - x) <= 0.45 * abs(x)
    end
  end

  property "align is idempotent" do
    # The boundaries are introduced because the invariant breaks for very large numbers.
    check all(x <- float(min: -1_000_000, max: 1_000_000)) do
      assert x |> FixAlign.align() == x |> FixAlign.align() |> FixAlign.align()
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

  # Need this because invariants break if the distance between the two numbers is large (presumably due to floating
  # point imprecision).
  @safe_distance 1_000_000

  defp int_interval do
    bind(integer(), fn x ->
      if x >= 0 do
        integer((x + 1)..(@safe_distance * max(x, 1)))
      else
        boundary = div(abs(x), @safe_distance)
        one_of([integer((x + 1)..-boundary), integer(boundary..abs(x * @safe_distance))])
      end
      |> map(fn y -> {x, y} end)
    end)
  end

  defp float_interval do
    bind(float(), fn x ->
      if x >= 0 do
        float(min: x, max: @safe_distance * with(0.0 <- x, do: 0.0000001))
      else
        boundary = abs(x) / @safe_distance
        one_of([float(min: x, max: -boundary), float(min: boundary, max: abs(x * @safe_distance))])
      end
      |> filter(&(&1 != x))
      |> map(fn y -> {x, y} end)
    end)
  end

  defp date_interval do
    gen all(
          {datetime1, datetime2} <- datetime_interval(),
          date1 = NaiveDateTime.to_date(datetime1),
          date2 = NaiveDateTime.to_date(datetime2),
          date1 != date2
        ) do
      {date1, date2}
    end
  end

  defp time_interval do
    gen all(
          {datetime1, datetime2} <- datetime_interval(),
          time1 = NaiveDateTime.to_time(datetime1),
          time2 = NaiveDateTime.to_time(datetime2),
          time1 != time2
        ) do
      [time1, time2]
      |> Enum.sort(&(Time.compare(&1, &2) != :gt))
      |> List.to_tuple()
    end
  end

  defp datetime_interval do
    gen all(datetime <- naive_datetime(), shift1 <- datetime_shift(), {unit, magnitude} <- datetime_shift()) do
      {Timex.shift(datetime, [{unit, -magnitude}]), Timex.shift(datetime, [shift1])}
    end
  end

  defp datetime_shift do
    {one_of([:seconds, :hours, :minutes, :days, :months, :years]), integer(1..120)}
  end

  def naive_datetime do
    gen all(date <- date(), time <- time()) do
      {:ok, naive_datetime} = NaiveDateTime.new(date, time)
      naive_datetime
    end
  end

  def date do
    gen all(
          year <- integer(1900..2100),
          month <- integer(1..12),
          day <- integer(1..31),
          result = Date.new(year, month, day),
          match?({:ok, _date}, result)
        ) do
      elem(result, 1)
    end
  end

  def time do
    gen all(hour <- integer(0..23), minute <- integer(0..59), second <- integer(0..59)) do
      Time.from_erl!({hour, minute, second})
    end
  end

  defp even_power_of_10?(x) do
    log = :math.log10(x)
    abs(round(log) - log) < epsilon()
  end

  defp even_multiple?(x, y) do
    abs(round(x / y) - x / y) <= abs(x / y * epsilon())
  end

  defp epsilon, do: 1.0e-6

  defp width({x, y}), do: y - x

  defp lt_eq(%Time{} = x, %Time{} = y), do: Cloak.Time.to_integer(x) <= Cloak.Time.to_integer(y)

  defp weak_lt_eq(x, y), do: x <= y or abs(x - y) < 0.0000005 * (x + y)

  defp legal_datetime?(value),
    do: value.year >= Cloak.Time.year_lower_bound() and value.year <= Cloak.Time.year_upper_bound()
end
