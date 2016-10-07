defmodule Cloak.Aql.FixAlign.Test do
  use ExUnit.Case, async: true
  use ExCheck

  alias Cloak.Aql.FixAlign

  for interval_type <- [:int, :float] do
    property "aligned #{interval_type} interval contains both ends of input" do
      for_all {x, y} in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align({x, y})
        left <= x && y <= right
      end
    end

    property "the endpoints of an #{interval_type} interval are multiples of half the size" do
      for_all interval in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align(interval)
        half_size = (right - left) / 2
        even_multiple?(left, half_size) && even_multiple?(right, half_size)
      end
    end

    property "the size of an aligned #{interval_type} interval is money-aligned" do
      for_all interval in interval(unquote(interval_type)) do
        {left, right} = FixAlign.align(interval)
        size = right - left
        even_power_of_10?(size) || even_power_of_10?(size / 2) || even_power_of_10?(size / 5)
      end
    end
  end

  property "fix align is idempotent on integer intervals" do
    for_all interval in int_interval do
      interval |> FixAlign.align() == interval |> FixAlign.align() |> FixAlign.align()
    end
  end

  property "an aligned interval is not much larger than the input" do
    for_all {x, y} in float_interval do
      interval = {x / 10, y / 10}
      10 * width(interval) >= interval |> FixAlign.align() |> width()
    end
  end

  defp interval(:int), do: int_interval
  defp interval(:float), do: float_interval

  defp int_interval, do: such_that({x, y} in {int, int} when x < y)

  defp float_interval, do: such_that({x, y} in {float, float} when x < y)

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
