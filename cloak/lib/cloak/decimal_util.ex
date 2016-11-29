defmodule Cloak.DecimalUtil do
  @moduledoc """
  Provides functions around the Decimal hex library.
  """


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Decimal.to_integer/1 crashes when there is loss of precision.
  This problem arises when the number being converted is has a negative
  exponent, and the coefficient doesn't divide cleanly by 10^(abs(exponent)).
  In reality there precision loss occurs outside of the desired precision,
  and hence is of no concern to us.
  This function reproduces the `Decimal.to_integer/1` functionality,
  but in a way that doesn't crash.
  """
  @decimal_precision :math.pow(10, 15)
  @spec to_precision(Decimal.t, float) :: float
  def to_precision(%Decimal{} = value, precision \\ @decimal_precision) do
    with_precision = case Decimal.mult(value, Decimal.new(precision)) do
      %Decimal{coef: coef, exp: exp} = value when exp < 0 ->
        rounder = trunc(:math.pow(10, abs(exp)))
        rounded_coef = Kernel.div(coef, rounder)
        %Decimal{value | coef: rounded_coef, exp: 0}
      value -> value
    end
    Decimal.to_integer(with_precision) / @decimal_precision
  end
end
