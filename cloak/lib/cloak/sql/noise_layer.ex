defmodule Cloak.Sql.NoiseLayer do
  @moduledoc "Contains functions for computing noise layer values."

  alias Cloak.Sql.{Expression, NoiseLayer.Normalizer}
  use Bitwise

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
  def new_accumulator(layers), do:
    Enum.map(layers, &MapSet.new([&1.base |> :erlang.term_to_binary() |> compute_hash()]))

  @doc "Adds the values from the given row to the noise layer accumulator."
  @spec accumulate([t], accumulator, Row.t) :: accumulator
  def accumulate(layers, accumulator, row) do
    for {layer, set} <- Enum.zip(layers, accumulator) do
      hash =
        layer.expressions
        |> Enum.map(&normalize(Expression.value(&1, row)))
        |> compute_hash()
      MapSet.put(set, hash)
    end
  end

  @doc "Combines the state of two noise layers accumulators into one."
  @spec merge_accumulators(accumulator, accumulator) :: accumulator
  def merge_accumulators(layers1, layers2), do:
    Stream.zip(layers1, layers2)
    |> Enum.map(fn ({hash_set1, hash_set2}) ->
      MapSet.union(hash_set1, hash_set2)
    end)

  @doc "Computes a cryptographic sum of the previously accumulated values, starting from the given salt."
  @spec sum(MapSet.t, String.t) :: integer()
  def sum(layer, salt), do:
    # since the list is not salt, using `xor` (which is commutative) will get us consistent results
    Enum.reduce(layer, compute_hash(salt), &bxor/2)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize(number) when is_number(number), do: Normalizer.normalize_number(number)
  defp normalize(nil), do: <<?N>>
  defp normalize(true), do: <<?T>>
  defp normalize(false), do: <<?F>>
  defp normalize(%Date{year: year, month: month, day: day}), do:
    <<?D, year::16, month::8, day::8>>
  defp normalize(%Time{hour: hour, minute: minute, second: second, microsecond: {microsecond, 6}}), do:
    <<?T, hour::8, minute::8, second::8, microsecond::16>>
  defp normalize(%NaiveDateTime{year: year, month: month, day: day,
      hour: hour, minute: minute, second: second, microsecond: {microsecond, 6}}), do:
    <<?D, ?T, year::16, month::8, day::8, hour::8, minute::8, second::8, microsecond::16>>
  defp normalize(data) when is_binary(data), do: data

  defp compute_hash(data) do
    <<a::16-binary, b::16-binary>> = :crypto.hash(:sha256, data)
    <<a::58-unsigned, b::58-unsigned, _c::12>> = :crypto.exor(a, b)
    a ^^^ b
  end
end
