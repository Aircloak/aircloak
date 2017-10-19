defmodule Cloak.Sql.NoiseLayer do
  @moduledoc "Contains functions for computing noise layer values."

  alias Cloak.Sql.{Expression, NoiseLayer.Normalizer}

  @type t :: %__MODULE__{base: any, expressions: [Expression.t]}
  @type accumulator :: [MapSet.t]

  defstruct [:base, :expressions]


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a noise layer with the given base data, based on the given list of expressions."
  @spec new(any, [Expression.t]) :: t
  def new(base, expressions), do: %__MODULE__{base: base, expressions: expressions}

  @doc "Returns an intial accumulator for gathering the values of a list of noise layers over a set of rows."
  @spec new_accumulator([t]) :: accumulator
  def new_accumulator(layers), do: Enum.map(layers, &MapSet.new([&1.base]))

  @doc "Adds the values from the given row to the noise layer accumulator."
  @spec accumulate([t], accumulator, Row.t) :: accumulator
  def accumulate(layers, accumulator, row) do
    for {layer, set} <- Enum.zip(layers, accumulator) do
      MapSet.put(set, Enum.map(layer.expressions, &normalize(Expression.value(&1, row))))
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize(number) when is_number(number), do: Normalizer.normalize_number(number, 6)
  defp normalize(data), do: data
end
