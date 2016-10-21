defmodule Cloak.Aql.FixAlign.Test do
  use ExUnit.Case, async: true
  use ExCheck

  alias Cloak.Aql.FixAlign

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

  property "aligned datetime interval contains both ends of the input" do
    for_all {x, y} in datetime_interval do
      {left, right} = FixAlign.align_interval({x, y})
      Timex.diff(x, left) >= 0 && Timex.diff(right, y) >= 0
    end
  end

  property "align_interval is idempotent on integer intervals" do
    for_all interval in int_interval do
      interval |> FixAlign.align_interval() == interval |> FixAlign.align_interval() |> FixAlign.align_interval()
    end
  end

  property "an aligned interval is not much larger than the input" do
    for_all {x, y} in float_interval do
      interval = {x / 10, y / 10}
      10 * width(interval) >= interval |> FixAlign.align_interval() |> width()
    end
  end

  property "numbers are money-aligned" do
    for_all x in (such_that y in float when y != 0) do
      result = x |> FixAlign.align() |> abs()
      even_power_of_10?(result) || even_power_of_10?(result / 2) || even_power_of_10?(result / 5)
    end
  end

  property "money-aligned numbers are close to the input" do
    for_all x in (such_that y in float when y != 0) do
      result = x |> FixAlign.align()
      abs(result - x) <= 0.45 * abs(x)
    end
  end

  property "align is idempotent" do
    for_all x in float do
      x |> FixAlign.align() == x |> FixAlign.align() |> FixAlign.align()
    end
  end

  test "rounds the mid-point up" do
    assert FixAlign.align(0.75) == 1
    assert FixAlign.align(35) == 50
    assert FixAlign.align(15) == 20
  end

  defp interval(:int), do: int_interval
  defp interval(:float), do: float_interval
  defp interval(:datetime), do: datetime_interval

  defp int_interval, do: such_that({x, y} in {int, int} when x < y)

  defp float_interval, do: such_that({x, y} in {float, float} when x < y)

  defp datetime_interval, do: such_that({x, y} in {datetime, datetime} when Timex.diff(x, y) < 0)

  defp datetime do
    domain(
      :datetime,
      fn(domain, size) ->
        size = size |> :math.pow(4) |> round()
        {domain, Timex.shift(~N[2000-06-15 12:20:30], seconds: draw(int, size))}
      end,
      fn(domain, item) -> {domain, item} end
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
end
