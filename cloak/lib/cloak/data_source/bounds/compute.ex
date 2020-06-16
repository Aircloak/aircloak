defmodule Cloak.DataSource.Bounds.Compute do
  @moduledoc "Implements computing column bounds giving a set of high/low values."

  import Kernel, except: [max: 2]

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns a money-aligned, safe upper bound for a column.

  The provided data should be the largest values for that column, with at most one value taken from each user.
  """
  @spec max([number], pos_integer) :: {:ok, integer()} | :error
  def max(data, bound_size_cutoff) do
    data
    |> Enum.sort(&Kernel.>/2)
    |> Enum.drop(bound_size_cutoff - 1)
    |> case do
      [] -> :error
      [number | _rest] -> {:ok, money_align_ceil(number)}
    end
  end

  @doc """
  Returns a money-aligned, safe lower bound for a column.

  The provided data should be the smallest values for that column, with at most one value taken from each user.
  """
  @spec min([number], pos_integer) :: {:ok, integer()} | :error
  def min(data, bound_size_cutoff) do
    with {:ok, result} <- data |> Enum.map(&(-&1)) |> max(bound_size_cutoff) do
      {:ok, -result}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp money_align_ceil(number) when number <= -1 do
    [-1, -2, -5]
    |> Stream.iterate(fn [a, b, c] -> [a * 10, b * 10, c * 10] end)
    |> Stream.flat_map(& &1)
    |> Stream.take_while(&(&1 >= number))
    |> Enum.at(-1)
  end

  defp money_align_ceil(number) when number < 0, do: 0
  defp money_align_ceil(number) when number < 1, do: 1

  defp money_align_ceil(number) do
    [1, 2, 5]
    |> Stream.iterate(fn [a, b, c] -> [a * 10, b * 10, c * 10] end)
    |> Stream.flat_map(& &1)
    |> Enum.find(&(&1 >= number))
  end
end
