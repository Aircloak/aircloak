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

  def many(complexity, generator) do
    if boolean(), do: many1(div(complexity, 2), generator), else: []
  end

  def many1(complexity, generator) do
    length = log_integer(complexity)

    for _ <- 1..length do
      generator.(div(complexity, length))
    end
  end

  def log_integer(complexity) do
    complexity |> max(1) |> :math.log2() |> trunc() |> uniform()
  end

  def boolean(), do: Enum.random([true, false])

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
end
