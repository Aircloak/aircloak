defmodule Cloak.Aql.FixAlign do
  @moduledoc "Implements fixing the alignment of ranges to a predetermined grid."

  @type interval :: {number, number}

  @epoch ~N[1970-01-01 00:00:00]
  @days_in_year 365
  @days_in_month 30
  @months_in_year 12


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
    baseline = order_of_magnitude(x)
    Enum.min_by([baseline * 10, baseline * 5, baseline * 2, baseline], fn(y) -> abs(x - y) end)
  end
  def align(_), do: 0

  @doc """
  Returns an interval that has been aligned to a fixed grid. The density of the grid depends on the size of
  the input interval. Both ends of the input will be contained inside the output.
  """
  @spec align_interval(interval) :: interval
  def align_interval({x, y}) when is_number(x) and is_number(y) and x > y, do: raise "Invalid interval"
  def align_interval(interval = {x, y}) when is_number(x) and is_number(y) do
    interval
    |> sizes()
    |> Stream.map(&snap(&1, interval))
    |> Enum.find(&(&1))
  end
  def align_interval({%NaiveDateTime{} = x, %NaiveDateTime{} = y}) do
    if Timex.diff(y, x) <= 0 do
      raise "Invalid interval"
    else
      align_date_time({x, y})
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp align_date_time({x, y}) do
    [:year, :month, :day, :hour, :minute, :second]
    |> Enum.find(fn(component) -> duration_component(component, Timex.diff(y, x, :duration)) >= 1 end)
    |> case do
      :year ->
        {x, y}
        |> year_since_epoch()
        |> align_interval()
        |> datetime_from_year()
      _ -> {x, y}
    end
  end

  defp year_since_epoch({x, y}) do
    if y == Timex.beginning_of_month(y) do
      {year_since_epoch(x), year_since_epoch(y)}
    else
      {year_since_epoch(x), year_since_epoch(%{y | month: y.month + 1})}
    end
  end
  defp year_since_epoch(%NaiveDateTime{year: year, month: month}) do
    year - @epoch.year + (month - @epoch.month) / @months_in_year
  end

  defp datetime_from_year({x, y}), do: {datetime_from_year(x), datetime_from_year(y)}
  defp datetime_from_year(x) do
    @epoch |> Timex.shift(months: round(@months_in_year * x))
  end

  defp duration_component(:year, duration), do: Timex.Duration.to_days(duration) / @days_in_year
  defp duration_component(:month, duration), do: Timex.Duration.to_days(duration) / @days_in_month
  defp duration_component(:day, duration), do: Timex.Duration.to_days(duration)
  defp duration_component(:hour, duration), do: Timex.Duration.to_hours(duration)
  defp duration_component(:minute, duration), do: Timex.Duration.to_minutes(duration)
  defp duration_component(:second, duration), do: Timex.Duration.to_seconds(duration)

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

  defp order_of_magnitude(x) do
    floor_log = x |> :math.log10() |> Float.floor()
    :math.pow(10, floor_log)
  end
end
