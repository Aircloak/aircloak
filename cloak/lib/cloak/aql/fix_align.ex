defmodule Cloak.Aql.FixAlign do
  def align(interval) do
    sizes()
    |> Stream.map(&snap(&1, interval))
    |> Enum.find(&(&1))
  end

  defp snap(size, {x, y}) do
    left = floor_to(x, size / 2)

    if y <= left + size, do: {left, left + size}, else: nil
  end

  defp floor_to(x, grid), do: Float.floor(x / grid) * grid

  defp sizes do
    Stream.iterate(1, &(&1 * 10))
    |> Stream.flat_map(&[&1, &1 * 2, &1 * 5])
  end
end
