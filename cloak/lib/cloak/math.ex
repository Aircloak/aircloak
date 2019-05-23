defmodule Cloak.Math do
  @moduledoc "Contains Cloak-specific math functions."

  @doc """
  Rounds a number to the given precision.

  Differs from `Float.round/2` in that:

  1. The precision can be negative, indicating rounding to tens, hundreds, etc.

    iex> Cloak.Math.round(125, -1)
    130.0

  2. It accepts integers as the first argument
  """
  def round(value, precision) when precision < 0,
    do: round(value * :math.pow(10, precision)) * :math.pow(10, -precision)

  def round(value, _precision) when is_integer(value), do: value
  def round(value, precision), do: Float.round(value, precision)

  @doc """
  Truncates the value to the given precision.

  A negative precision signifies truncating to tens, hundreds, etc. For example:

      iex> Cloak.Math.trunc(123, -1)
      120.0
  """
  def trunc(value, 0), do: trunc(value)

  def trunc(value, precision) when precision < 0,
    do: trunc(value * :math.pow(10, precision)) * :math.pow(10, -precision)

  def trunc(value, _precision) when is_integer(value), do: value
  def trunc(value, precision) when value < 0, do: value |> :erlang.float() |> Float.ceil(precision)
  def trunc(value, precision), do: value |> :erlang.float() |> Float.floor(precision)
end
