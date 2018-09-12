defmodule Cloak.Compliance.QueryGenerator.Generation do
  @moduledoc "Contains helper macros and functions to aid in generating random ASTs."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Select a random option from the given list of `{weight, option}` pairs. The options will be evaluated lazily. The list
  will be logically cut off after a total weight of `complexity`.
  """
  defmacro frequency(complexity, options) do
    options =
      Enum.map(options, fn {weight, val} ->
        {weight,
         quote do
           fn -> unquote(val) end
         end}
      end)

    quote do
      unquote(__MODULE__).do_frequency(unquote(complexity), unquote(options))
    end
  end

  @doc """
  Take values from the generator until one not matching the predicate is found. Raises an error if no such value is
  found after `attempts` attempts.
  """
  @spec reject((() -> x), (x -> boolean), non_neg_integer) :: x when x: var
  def reject(generator, predicate, attempts \\ 100)
  def reject(_generator, _predicate, 0), do: raise("Filter too narrow")

  def reject(generator, predicate, counter) do
    value = generator.()

    if predicate.(value) do
      reject(generator, predicate, counter - 1)
    else
      value
    end
  end

  @doc "Same as `many1/2` but the list can have 0 elements as well."
  @spec many(non_neg_integer, (non_neg_integer -> x)) :: [x] when x: var
  def many(complexity, generator) do
    if boolean(), do: many1(div(complexity, 2), generator), else: []
  end

  @doc """
  Generate a list of 1 or more values taken from the generator. The list will be longer if the complexity is larger. The
  generator will be passed a reduced complexity. The reduction is proportional to the length of the generated list.
  """
  @spec many1(non_neg_integer, (non_neg_integer -> x)) :: [x] when x: var
  def many1(complexity, generator) do
    length = log_integer(complexity)

    for _ <- 1..length do
      generator.(div(complexity, length))
    end
  end

  @doc "Generate a random boolean."
  @spec boolean() :: boolean
  def boolean(), do: Enum.random([true, false])

  @doc "Generate a random integer. The integer will be drawn from the range of 1..max. It will be 1 if max is 0."
  @spec uniform(non_neg_integer) :: pos_integer
  def uniform(max) when max < 1, do: 1
  def uniform(max), do: :rand.uniform(max)

  # -------------------------------------------------------------------
  # Macro implementation
  # -------------------------------------------------------------------

  @doc false
  def do_frequency(complexity, options) do
    sum = options |> Enum.map(&elem(&1, 0)) |> Enum.sum()
    random = uniform(min(sum, complexity)) - 1
    pick_option(options, random)
  end

  defp pick_option([{frequency, option} | _], number) when number < frequency, do: option.()
  defp pick_option([{frequency, _} | options], number), do: pick_option(options, number - frequency)

  defp log_integer(complexity) do
    complexity |> max(1) |> :math.log2() |> trunc() |> uniform()
  end
end
