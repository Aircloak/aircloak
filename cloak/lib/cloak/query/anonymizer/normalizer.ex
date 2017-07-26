defmodule Cloak.Query.Anonymizer.Normalizer do
  @moduledoc """
  Normalizes values for producing consistent seeds.
  """


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Takes a float value and normalizes it such that it only retains a set
  number of significant digits.
  The returned value should be considered opaque, and you should
  not expect it to be of a certain form. It is meant to be feed into
  a hashing function for the purpose of generating a seed.

  The function will ensure that similar looking numbers like 1.2345 and 12.345
  produce different results.
  """
  @spec normalize_float(float, non_neg_integer) :: float
  def normalize_float(number, significant_digits), do:
    {normalize_float(abs(number), significant_digits, 0), number < 0}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_float(number, n, exponent) when number >= 1 and number < 10, do:
    {Float.round(number, n - 1), exponent}
  defp normalize_float(number, n, exponent) when number >= 10, do:
    normalize_float(number / 10, n, exponent + 1)
  defp normalize_float(number, n, exponent) when number < 1, do:
    normalize_float(number * 10, n, exponent - 1)

  # When we have divided a number down to having one digit before the comma sign (10.1 -> 1.01)
  # we also need to keep additional significant digits, to retain the original desired number of
  # significant digits. I.e. if we wanted to 1 significant digits, and went from 10.1 to 1.01,
  # then we need to retain both the 0 and the 1 after the comma, as opposed to just the 0.
  defp significant_digits(significant_digits, exponent) when exponent > 0, do: significant_digits + exponent
  defp significant_digits(significant_digits, _exponent), do: significant_digits
end
