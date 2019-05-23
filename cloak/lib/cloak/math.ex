defmodule Cloak.Math do
  @moduledoc "Contains Cloak-specific math functions."

  @doc """
  Rounds a number to the given precision.

  Differs from `Float.round/2` in that:

  1. The precision can be negative, indicating rounding to tens, hundreds, etc
  2. It accepts integers as the first argument
  """
  def round(value, precision) when precision < 0,
    do: round(value * :math.pow(10, precision)) * :math.pow(10, -precision)

  def round(value, _precision) when is_integer(value), do: value
  def round(value, precision), do: Float.round(value, precision)
end
