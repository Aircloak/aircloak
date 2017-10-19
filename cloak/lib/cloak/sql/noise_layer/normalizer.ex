defmodule Cloak.Sql.NoiseLayer.Normalizer do
  @moduledoc """
  Normalizes values for producing consistent seeds.
  """


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Takes a numeric value and normalizes it such that it only retains a set
  number of significant digits.
  The returned value should be considered opaque, and you should
  not expect it to be of a certain form. It is meant to be fed into
  a hashing function for the purpose of generating a seed.

  The function will ensure that similar looking numbers like 1.2345 and 12.345
  produce different results. It will also return the same result for floats
  and integers denoting the same number, like 1 and 1.0.
  """
  @spec normalize_number(number, non_neg_integer) :: term
  def normalize_number(number, significant_digits), do:
    {normalize_float(abs(:erlang.float(number)), significant_digits, 0), number < 0}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_float(0.0, _n, exponent), do:
    {0.0, exponent}
  defp normalize_float(number, n, exponent) when number >= 1 and number < 10, do:
    {Float.round(number, n - 1), exponent}
  defp normalize_float(number, n, exponent) when number >= 10, do:
    normalize_float(number / 10, n, exponent + 1)
  defp normalize_float(number, n, exponent) when number < 1, do:
    normalize_float(number * 10, n, exponent - 1)
end
