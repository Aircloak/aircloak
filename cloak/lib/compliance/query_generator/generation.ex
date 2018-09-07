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

  # -------------------------------------------------------------------
  # Macro implementation
  # -------------------------------------------------------------------

  @doc false
  def do_frequency(complexity, options) do
    sum = options |> Enum.map(&elem(&1, 0)) |> Enum.sum()
    random = :rand.uniform(sum |> min(complexity) |> max(1)) - 1
    pick_option(options, random)
  end

  defp pick_option([{frequency, option} | _], number) when number < frequency, do: option.()
  defp pick_option([{frequency, _} | options], number), do: pick_option(options, number - frequency)
end
