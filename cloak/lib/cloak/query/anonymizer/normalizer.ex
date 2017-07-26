defmodule Cloak.Query.Anonymizer.Normalizer do
  @moduledoc """
  Normalizes values for producing consistent seeds.
  """


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Takes a float value and normalizes it such that it has a set number of
  the original significant digits present. For example, the values 1.23456 and 1.23466
  would appear identical after normalization if 3 significant digits were
  used, but different when 4 significant digits were used.

  Furthermore it is ensured that similar looking numbers like 1.2345 and 12.345
  produce different results.
  """
  @spec normalize_float(float, non_neg_integer) :: float
  def normalize_float(number, significant_digis), do:
    normalize_float(abs(number), significant_digis, 0, number < 0)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_float(number, n, change, was_negative) when number >= 1 and number < 10 do
    adjusted_number = Float.round(
      adjust_number_for_change(number, change),
      significant_digits(n, change)
    )
    if was_negative do
      -1 * adjusted_number
    else
      adjusted_number
    end
  end
  defp normalize_float(number, n, change, was_negative) when number >= 10, do:
    normalize_float(number / 10, n, change - 1, was_negative)
  defp normalize_float(number, n, change, was_negative) when number < 1, do:
    normalize_float(number * 10, n, change + 1, was_negative)

  # With a high probability this will ensure that similar looking numbers end up different.
  defp adjust_number_for_change(number, change) when change < 0, do: number + 199 * abs(change)
  defp adjust_number_for_change(number, change), do: number + 197 * change

  # When we have divided a number down to having one digit before the comma sign (10.1 -> 1.01)
  # we also need to keep additional significant digits, to retain the original desired number of
  # significant digits. I.e. if we wanted to 1 significant digits, and went from 10.1 to 1.01,
  # then we need to retain both the 0 and the 1 after the comma, as opposed to just the 0.
  defp significant_digits(n, change) when change < 0, do: n + abs(change)
  defp significant_digits(n, _change), do: n
end
