defmodule Cloak.Aql.FixAlign do
  def align(interval) do
    interval
    |> sizes()
    |> Stream.map(&snap(&1, interval))
    |> Enum.find(&(&1))
  end

  defp snap(size, {x, y}) do
    left = floor_to(x, size / 2)

    if y <= left + size, do: {left, left + size}, else: nil
  end

  defp floor_to(x, grid), do: Float.floor(x / grid) * grid

  defp sizes(interval) do
    Stream.concat(small_sizes(interval), large_sizes)
    |> Stream.flat_map(&[&1, &1 * 2, &1 * 5])
  end

  defp small_sizes({x, y}) do
    start =
      (y - x)
      |> :math.log10()
      |> Float.floor()
      |> round()

    if start < 0, do: Stream.map(start .. -1, &(:math.pow(10, &1))), else: []
  end

  defp large_sizes, do: Stream.iterate(1, &(&1 * 10))
end
