defmodule Cloak.Aql.FixAlign.Test do
  use ExUnit.Case, async: true
  use ExCheck

  alias Cloak.Aql.FixAlign

  property "aligned interval contains both ends of input" do
    for_all {x, y} in int_interval do
      {left, right} = FixAlign.align({x, y})
      left <= x && y <= right
    end
  end

  property "the endpoints are multiples of half the size" do
    for_all interval in int_interval do
      {left, right} = FixAlign.align(interval)
      half_size = (right - left) / 2
      even_multiple?(left, half_size) && even_multiple?(right, half_size)
    end
  end

  property "the size is an even power of ten, 2 times an even power or 5 times an even power (money-aligned)" do
    for_all interval in int_interval do
      {left, right} = FixAlign.align(interval)
      size = right - left
      even_power_of_10?(size) || even_power_of_10?(size / 2) || even_power_of_10?(size / 5)
    end
  end

  property "fix align is idempotent" do
    for_all interval in int_interval do
      FixAlign.align(interval) == FixAlign.align(FixAlign.align(interval))
    end
  end

  defp int_interval, do: such_that({x, y} in {int, int} when x < y)

  defp even_power_of_10?(x) do
    log = :math.log10(x)
    abs(round(log) - log) < epsilon()
  end

  defp even_multiple?(x, y) do
    abs(round(x / y) - (x / y)) <= abs((x / y) * epsilon())
  end

  defp epsilon, do: 1.0e-6
end
