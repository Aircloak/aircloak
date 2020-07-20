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
  @spec round(number(), integer()) :: number()
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
  @spec trunc(number(), integer()) :: number()
  def trunc(value, 0), do: trunc(value)

  def trunc(value, precision) when precision < 0,
    do: trunc(value * :math.pow(10, precision)) * :math.pow(10, -precision)

  def trunc(value, _precision) when is_integer(value), do: value
  def trunc(value, precision) when value < 0, do: value |> :erlang.float() |> Float.ceil(precision)
  def trunc(value, precision), do: value |> :erlang.float() |> Float.floor(precision)

  @doc """
  Computes a to the bth power.

  Differs from `:math.pow/2` in that it assumes the base is an integer and the exponent is a non-negative integer.
  Consequently, it produces an integer result with unlimited precision.

      iex> Cloak.Math.int_pow(12, 20)
      3833759992447475122176
  """
  @spec int_pow(integer(), non_neg_integer()) :: integer()
  def int_pow(_base, 0), do: 1

  def int_pow(base, exponent) do
    partial = int_pow(base, div(exponent, 2))
    if(rem(exponent, 2) == 0, do: partial * partial, else: partial * partial * base)
  end

  @doc "Maximum magnitude of numeric values."
  @spec numeric_max_scale() :: pos_integer()
  # Maximum number of digits a 64-bit integer can contain.
  def numeric_max_scale(), do: 18
end
