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
    number |> :erlang.float() |> normalize_float()


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # We want to keep only the sign bit, the exponent and 6 significant decimal digits
  # The format of a 64-bit float is [1-bit sign, 11-bit exponent, 52-bit mantissa]
  # Since 10^6 ~= 2^20, it means we roughly need to keep the first half of number only.
  defp normalize_float(number) do
    <<normalized::4-binary, _dropped_digits::4-binary>> = <<number::64-float>>
    normalized
  end
end
