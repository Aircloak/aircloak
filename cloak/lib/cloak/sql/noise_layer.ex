defmodule Cloak.Sql.NoiseLayer do
  @moduledoc "Contains functions for computing noise layer values."

  alias Cloak.Sql.{Expression, NoiseLayer.Normalizer}
  use Bitwise

  @type t :: %__MODULE__{base: any, expressions: [Expression.t()]}
  @type hash_set :: MapSet.t()
  @type accumulator :: [hash_set]

  defstruct [:base, :expressions]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a noise layer with the given base data, based on the given list of expressions."
  @spec new(any, [Expression.t()]) :: t
  def new(base, expressions), do: %__MODULE__{base: base, expressions: expressions}

  @doc "Returns an intial accumulator for gathering the values of a list of noise layers over a set of rows."
  @spec new_accumulator([t]) :: accumulator
  def new_accumulator(layers),
    do: Enum.map(layers, &MapSet.new([&1.base |> :erlang.term_to_binary() |> compute_hash()]))

  @doc "Pre-processes the noise layers so that expressions are computed only once during accumulation."
  @spec pre_process_layers([t]) :: {[Expression.t()], [[integer()]]}
  def pre_process_layers(layers) do
    unique_expressions = layers |> Enum.flat_map(& &1.expressions) |> Enum.uniq()
    indices_list = Enum.map(layers, &expressions_to_indices(&1.expressions, unique_expressions))
    {unique_expressions, indices_list}
  end

  @doc "Adds the values from the given row to the noise layer accumulator."
  @spec accumulate({[Expression.t()], [[integer()]]}, accumulator, Row.t()) :: accumulator
  def accumulate({unique_expressions, indices_list}, accumulator, row) do
    values = Enum.map(unique_expressions, &normalize(Expression.value(&1, row)))

    for {indices, set} <- Enum.zip(indices_list, accumulator) do
      hash =
        indices
        |> Enum.map(&Enum.at(values, &1))
        |> compute_hash()

      MapSet.put(set, hash)
    end
  end

  @doc "Combines the state of two noise layers accumulators into one."
  @spec merge_accumulators(accumulator, accumulator) :: accumulator
  def merge_accumulators(accumulator1, accumulator2),
    do:
      Stream.zip(accumulator1, accumulator2)
      |> Enum.map(fn {hash_set1, hash_set2} ->
        MapSet.union(hash_set1, hash_set2)
      end)

  @doc "Computes a cryptographic sum of the previously accumulated values, starting from the given salt."
  @spec sum(hash_set, String.t()) :: integer()
  @dialyzer {:nowarn_function, sum: 2}
  def sum(hash_set = %MapSet{}, salt),
    # since the list is not sorted, using `xor` (which is commutative) will get us consistent results
    do: Enum.reduce(hash_set, compute_hash(salt), &bxor/2)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize(number) when is_number(number), do: Normalizer.normalize_number(number)
  defp normalize(nil), do: <<?N>>
  defp normalize(true), do: <<?T>>
  defp normalize(false), do: <<?F>>

  defp normalize(%Date{year: year, month: month, day: day}),
    do: <<?D, year::16, month::8, day::8>>

  defp normalize(%Time{hour: hour, minute: minute, second: second, microsecond: {microsecond, 6}}),
    do: <<?T, hour::8, minute::8, second::8, microsecond::16>>

  defp normalize(%NaiveDateTime{
         year: year,
         month: month,
         day: day,
         hour: hour,
         minute: minute,
         second: second,
         microsecond: {microsecond, 6}
       }),
       do: <<?D, ?T, year::16, month::8, day::8, hour::8, minute::8, second::8, microsecond::16>>

  defp normalize(data) when is_binary(data), do: data

  defp compute_hash(data) do
    <<a::16-binary, b::16-binary>> = :crypto.hash(:sha256, data)
    <<a::58-unsigned, b::58-unsigned, _c::12>> = :crypto.exor(a, b)
    a ^^^ b
  end

  defp expressions_to_indices(layer_expressions, unique_expressions),
    do: Enum.map(layer_expressions, &expression_index(&1, unique_expressions))

  defp expression_index(expression, expressions),
    do: Enum.find_index(expressions, &(&1 == expression))
end
