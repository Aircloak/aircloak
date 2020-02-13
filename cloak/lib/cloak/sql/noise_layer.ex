defmodule Cloak.Sql.NoiseLayer do
  @moduledoc "Contains functions for computing noise layer values."

  alias Cloak.Sql.{Expression, NoiseLayer.Normalizer}
  use Bitwise

  @type tag :: {:grouping_set | :aggregator, pos_integer()} | nil
  @type t :: %__MODULE__{base: any, expressions: [Expression.t()], tag: tag}
  @type hash_set :: MapSet.t()
  @type accumulator :: [{tag, hash_set}]
  @type processed :: {[Expression.t()], [any], [[integer()]]}

  defstruct [:base, :expressions, :tag]

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(noise_layer, opts) do
      data = %{
        base: noise_layer.base,
        expressions: Enum.map(noise_layer.expressions, &Expression.display/1),
        tag: noise_layer.tag
      }

      concat(["#NoiseLayer<", to_doc(data, opts), ">"])
    end
  end

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns a noise layer with the given base data, based on the given list of expressions."
  @spec new(any, [Expression.t()], tag) :: t
  def new(base, expressions, tag \\ nil),
    do: %__MODULE__{base: base, expressions: expressions, tag: tag}

  @doc "Returns an intial accumulator for gathering the values of a list of noise layers over a set of rows."
  @spec new_accumulator([t]) :: accumulator
  def new_accumulator(layers),
    do: Enum.map(layers, &{&1.tag, MapSet.new([&1.base |> :erlang.term_to_binary() |> compute_hash()])})

  @doc "Pre-processes the noise layers so that expressions are computed only once during accumulation."
  @spec pre_process_layers([t]) :: processed
  def pre_process_layers(layers) do
    unique_expressions = layers |> Enum.flat_map(& &1.expressions) |> Enum.uniq()
    {constant_expressions, dynamic_expressions} = Enum.split_with(unique_expressions, &Expression.constant?/1)
    unique_expressions = dynamic_expressions ++ constant_expressions
    indices_list = Enum.map(layers, &expressions_to_indices(&1.expressions, unique_expressions))
    constants = constant_expressions |> Enum.map(&Expression.const_value/1) |> Enum.map(&normalize/1)
    {dynamic_expressions, constants, indices_list}
  end

  @doc "Returns only the noise layers that apply to the specified grouping set."
  @spec filter_layers_for_grouping_set([t], pos_integer() | nil) :: [t]
  def filter_layers_for_grouping_set(layers, index) do
    Enum.filter(layers, fn
      %__MODULE__{tag: {:grouping_set, ^index}} -> true
      %__MODULE__{tag: {:grouping_set, _}} -> false
      _ -> true
    end)
  end

  @doc "Returns only the accumulated noise layers that apply to the specified aggregator."
  @spec filter_accumulator_for_aggregator(accumulator, pos_integer() | nil) :: accumulator
  def filter_accumulator_for_aggregator(accumulator, index) do
    Enum.filter(accumulator, fn
      {{:aggregator, ^index}, _hash_set} -> true
      {{:aggregator, _}, _hash_set} -> false
      _ -> true
    end)
  end

  @doc "Returns true if there are custom accumulated noise layers for the specified aggregator."
  @spec accumulator_references_aggregator?(accumulator, pos_integer()) :: boolean
  def accumulator_references_aggregator?(accumulator, index),
    do: Enum.any?(accumulator, fn {tag, _hahs_set} -> tag == {:aggregator, index} end)

  @doc "Adds the values from the given row to the noise layer accumulator."
  @spec accumulate(processed, accumulator, Row.t()) :: accumulator
  def accumulate({dynamic_expressions, constants, indices_list}, accumulator, row) do
    values = Enum.map(dynamic_expressions, &normalize(Expression.value(&1, row))) ++ constants

    for {indices, {tag, set}} <- Enum.zip(indices_list, accumulator) do
      hash =
        indices
        |> Enum.map(&Enum.at(values, &1))
        |> compute_hash()

      {tag, MapSet.put(set, hash)}
    end
  end

  @doc "Combines the state of two noise layers accumulators into one."
  @spec merge_accumulators(accumulator, accumulator) :: accumulator
  def merge_accumulators(accumulator1, accumulator2),
    do:
      Stream.zip(accumulator1, accumulator2)
      |> Enum.map(fn {{tag, hash_set1}, {tag, hash_set2}} ->
        {tag, MapSet.union(hash_set1, hash_set2)}
      end)

  @doc "Computes a cryptographic sum of the previously accumulated values, starting from the given salt."
  @spec sum_hashes({tag, MapSet.t()}, String.t()) :: integer()
  def sum_hashes({_tag, hash_set}, salt),
    # since the list is not sorted, using `xor` (which is commutative) will get us consistent results
    do: Enum.reduce(hash_set, compute_hash(salt), &bxor/2)

  @doc "Generates a noise layer accumulator from the given values."
  @spec accumulator_from_values([Cloak.Data.t()]) :: accumulator
  def accumulator_from_values(values),
    do: [{nil, values |> Enum.map(&normalize/1) |> Enum.map(&compute_hash/1) |> MapSet.new()}]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize(number) when is_number(number), do: Normalizer.normalize_number(number)
  defp normalize(nil), do: <<?N>>
  defp normalize(true), do: <<?T>>
  defp normalize(false), do: <<?F>>

  defp normalize(%Date{year: year, month: month, day: day}), do: <<?D, year::16, month::8, day::8>>

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
    # We only keep 58-bits of data for the RNG seed, so there is no point in using a hash function with longer output.
    # From the tested cryptographic hash functions (MD5, SHA1 and SHA2), SHA1 executes the fastest.
    # xxHash was also tested (which is not cryptographically secure), and it runs only 4% faster than SHA1.
    <<a::58-unsigned, b::58-unsigned, _c::44>> = :crypto.hash(:sha, data)
    a ^^^ b
  end

  defp expressions_to_indices(layer_expressions, unique_expressions),
    do: Enum.map(layer_expressions, &expression_index(&1, unique_expressions))

  defp expression_index(expression, expressions), do: Enum.find_index(expressions, &(&1 == expression))
end
