defmodule Cloak.Aql.FixAlign do
  @moduledoc "Implements fixing the alignment of ranges to a predetermined grid."

  @type interval :: {number, number}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns the closest money-aligned number (from the sequence ..., 0.1, 0.2, 0.5, 1, 2, 5, 10, ...). Returns
  0 for 0. Returns -align(-x) for negative x.
  """
  @spec align(number) :: number
  def align(x) when x < 0, do: -align(-x)
  def align(x) when x > 0 do
    baseline = log_round(10, x)
    Enum.min_by([baseline, baseline * 2, baseline * 5, baseline * 10], fn(y) -> abs(x - y) end)
  end
  def align(_), do: 0

  @doc """
  Returns an interval that has been aligned to a fixed grid. The density of the grid depends on the size of
  the input interval. Both ends of the input will be contained inside the output.
  """
  @spec align_interval(interval) :: interval
  def align_interval({x, y}) when x > y, do: raise "Invalid interval"
  def align_interval(interval) do
    interval
    |> sizes()
    |> Stream.map(&snap(&1, interval))
    |> Enum.find(&(&1))
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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

  defp log_round(base, x) do
    floor_log = x |> :math.log10() |> Float.floor()
    :math.pow(base, floor_log)
  end
end
