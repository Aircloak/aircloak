defmodule Cloak.Aql.FixAlign do
  @moduledoc "Implements fixing the alignment of ranges to a predetermined grid."

  @type interval :: {number, number}

  @epoch ~N[1970-01-01 00:00:00]
  @time_units [:year, :month, :day, :hour, :minute, :second]
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

  defp align_date_time(interval), do: interval |> align_date_time_once() |> align_date_time_once()

  defp align_date_time_once({x, y}) do
    @time_units
    |> Enum.find(fn(component) -> duration_component(component, Timex.diff(y, x, :duration)) >= 1 end)
    |> case do
      :year ->
        {x, y}
        |> units_since_epoch(:year)
        |> align_interval()
        |> datetime_from_units(:year)
      :month ->
        {x, y}
        |> units_since_epoch(:month)
        |> align_interval()
        |> datetime_from_units(:month)
      _ -> {x, y}
    end
  end

  defp units_since_epoch({x, y}, unit), do:
    {units_since_epoch(x, unit), y |> datetime_ceil(lower_unit(unit)) |> units_since_epoch(unit)}
  defp units_since_epoch(%NaiveDateTime{year: year, month: month}, :year), do:
    year - @epoch.year + (month - @epoch.month) / @months_in_year
  defp units_since_epoch(%NaiveDateTime{year: year, month: month, day: day}, :month), do:
    (year - @epoch.year) * @months_in_year + (month - @epoch.month) + (day - @epoch.day) / @days_in_month

  defp datetime_ceil(datetime, :month), do:
    if datetime == Timex.beginning_of_month(datetime),
      do: datetime,
      else: %{datetime | month: datetime.month + 1, day: 1, hour: 0, minute: 0, second: 0}
  defp datetime_ceil(datetime, :day), do:
    if datetime == Timex.beginning_of_day(datetime),
      do: datetime,
      else: %{datetime | day: datetime.day + 1, hour: 0, minute: 0, second: 0}

  defp lower_unit(:second), do: :second
  defp lower_unit(unit), do: Enum.at(@time_units, Enum.find_index(@time_units, &(&1 == unit)) + 1)

  defp datetime_from_units({x, y}, unit), do: {datetime_from_units(x, unit), datetime_from_units(y, unit)}
  defp datetime_from_units(x, :year), do: @epoch |> Timex.shift(months: round(@months_in_year * x))
  defp datetime_from_units(x, :month) do
    days = (x - Float.floor(x)) * @days_in_month |> round()
    months = x |> Float.floor() |> round()
    Timex.shift(@epoch, months: months, days: days)
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
