defmodule Cloak.Sql.NoiseLayer do
  @moduledoc "Contains functions for computing noise layer values."

  alias Cloak.Sql.Expression

  @type t :: %__MODULE__{name: String.t, expressions: [Expression.t]}
  @type accumulator :: [MapSet.t]

  defstruct [:name, :expressions]

  @doc "Returns a noise layer with the given name, based on the given list of expressions."
  @spec new(String.t, [Expression.t]) :: t
  def new(name, expressions), do: %__MODULE__{name: name, expressions: expressions}

  @doc "Returns an intial accumulator for gathering the values of a list of noise layers over a set of rows."
  @spec new_accumulator([t]) :: accumulator
  def new_accumulator(layers), do: Enum.map(layers, &MapSet.new([&1.name]))

  @doc "Adds the values from the given row to the noise layer accumulator."
  @spec accumulate([t], accumulator, Row.t) :: accumulator
  def accumulate(layers, accumulator, row) do
    for {layer, set} <- Enum.zip(layers, accumulator) do
      MapSet.put(set, Enum.map(layer.expressions, &Expression.value(&1, row)))
    end
  end
end
