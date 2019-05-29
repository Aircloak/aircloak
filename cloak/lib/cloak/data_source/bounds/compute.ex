defmodule Cloak.DataSource.Bounds.Compute do
  import Kernel, except: [max: 2]

  def max(data, bound_size_cutoff) do
    data
    |> Enum.sort(&Kernel.>/2)
    |> Enum.drop(bound_size_cutoff - 1)
    |> case do
      [] -> :error
      [number | _rest] -> {:ok, lteq_money_aligned(number)}
    end
  end

  def min(data, bound_size_cutoff) do
    with {:ok, result} <- data |> Enum.map(&(-&1)) |> max(bound_size_cutoff) do
      {:ok, -result}
    end
  end

  defp lteq_money_aligned(number) when number < 0 do
    [-1, -2, -5]
    |> Stream.iterate(fn [a, b, c] -> [a * 10, b * 10, c * 10] end)
    |> Stream.flat_map(& &1)
    |> Enum.find(&(&1 <= number))
  end

  defp lteq_money_aligned(number) when number <= 1, do: 0

  defp lteq_money_aligned(number) do
    [1, 2, 5]
    |> Stream.iterate(fn [a, b, c] -> [a * 10, b * 10, c * 10] end)
    |> Stream.flat_map(& &1)
    |> Enum.take_while(&(&1 <= number))
    |> List.last()
  end
end
