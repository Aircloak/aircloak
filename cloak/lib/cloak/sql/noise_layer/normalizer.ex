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
  @spec normalize_number(number) :: term
  def normalize_number(number), do:
    <<sign(number), normalize_float(abs(:erlang.float(number)), _significant_digits = 6, 0)::binary>>


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sign(number) when number < 0, do: ?-
  defp sign(number) when number >= 0, do: ?+

  defp normalize_float(0.0, _n, exponent), do:
    <<exponent::8, 0::64-float>>
  defp normalize_float(number, n, exponent) when number >= 1 and number < 10, do:
    <<exponent::8, Float.round(number, n - 1)::64-float>>
  defp normalize_float(number, n, exponent) when number >= 10, do:
    normalize_float(number / 10, n, exponent + 1)
  defp normalize_float(number, n, exponent) when number < 1, do:
    normalize_float(number * 10, n, exponent - 1)
end
