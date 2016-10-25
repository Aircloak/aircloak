defmodule Cloak.Aql.FixAlign do
  @moduledoc "Implements fixing the alignment of ranges to a predetermined grid."

  @type interval :: {number, number}

  @default_size_factors [1, 2, 5]
  @epoch ~N[1970-01-01 00:00:00]
  @time_units [:years, :months, :days, :hours, :minutes, :seconds]
  @months_in_year 12
  @days_in_year 365
  @days_in_month 30
  @hours_in_day 24
  @minutes_in_hour 60
  @seconds_in_minute 60


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
  @spec align_interval(interval, [number]) :: interval
  def align_interval(interval, size_factors \\ @default_size_factors)
  def align_interval({x, y}, _) when is_number(x) and is_number(y) and x > y, do: raise "Invalid interval"
  def align_interval(interval = {x, y}, size_factors) when is_number(x) and is_number(y) do
    interval
    |> sizes(size_factors)
    |> Stream.map(&snap(&1, interval))
    |> Enum.find(&(&1))
  end
  def align_interval({%NaiveDateTime{} = x, %NaiveDateTime{} = y}, _) do
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
    unit = @time_units
    |> Enum.find(fn(component) -> duration_component(component, Timex.diff(y, x, :duration)) >= 1 end)

    {x, y}
    |> units_since_epoch(unit)
    |> align_interval(size_factors(unit))
    |> datetime_from_units(unit)
  end

  defp size_factors(:years), do: @default_size_factors
  defp size_factors(:months), do: [1, 2, 6]
  defp size_factors(:days), do: @default_size_factors
  defp size_factors(:hours), do: [1, 2, 6, 12, 24]
  defp size_factors(:minutes), do: [1, 2, 5, 15, 30, 60]
  defp size_factors(:seconds), do: [1, 2, 5, 15, 30, 60]

  defp units_since_epoch({x, y}, unit), do:
    {units_since_epoch(x, unit), y |> datetime_ceil(lower_unit(unit)) |> units_since_epoch(unit)}
  defp units_since_epoch(%NaiveDateTime{year: year, month: month}, :years), do:
    year - @epoch.year + (month - @epoch.month) / @months_in_year
  defp units_since_epoch(%NaiveDateTime{year: year, month: month, day: day}, :months), do:
    (year - @epoch.year) * @months_in_year + (month - @epoch.month) + (day - @epoch.day) / @days_in_month
  defp units_since_epoch(datetime, :days), do:
    Timex.diff(datetime, @epoch, :days) + datetime.hour / @hours_in_day
  defp units_since_epoch(datetime, :hours), do:
    Timex.diff(datetime, @epoch, :hours) + datetime.minute / @minutes_in_hour
  defp units_since_epoch(datetime, :minutes), do:
    Timex.diff(datetime, @epoch, :minutes) + datetime.second / @seconds_in_minute
  defp units_since_epoch(datetime, :seconds), do: Timex.diff(datetime, @epoch, :seconds)

  defp datetime_ceil(datetime, :months), do:
    if datetime == Timex.beginning_of_month(datetime),
      do: datetime,
      else: %{datetime | month: datetime.month + 1, day: 1, hour: 0, minute: 0, second: 0}
  defp datetime_ceil(datetime, :days), do:
    if datetime == Timex.beginning_of_day(datetime),
      do: datetime,
      else: %{datetime | day: datetime.day + 1, hour: 0, minute: 0, second: 0}
  defp datetime_ceil(datetime = %{minute: 0, second: 0}, :hours), do: datetime
  defp datetime_ceil(datetime, :hours), do: %{datetime | hour: datetime.hour + 1, minute: 0, second: 0}
  defp datetime_ceil(datetime = %{second: 0}, :minutes), do: datetime
  defp datetime_ceil(datetime, :minutes), do: %{datetime | minute: datetime.minute + 1, second: 0}
  defp datetime_ceil(datetime, :seconds), do: datetime

  defp datetime_from_units({x, y}, unit), do: {datetime_from_units(x, unit), datetime_from_units(y, unit)}
  # defp datetime_from_units(x, :years), do: @epoch |> Timex.shift(months: round(@months_in_year * x))
  defp datetime_from_units(x, unit) do
    less_significant = (x - Float.floor(x)) * conversion_factor(unit, lower_unit(unit)) |> round()
    more_significant = x |> Float.floor() |> round()
    Timex.shift(@epoch, [{unit, more_significant}, {lower_unit(unit), less_significant}])
  end

  defp conversion_factor(:years, :months), do: @months_in_year
  defp conversion_factor(:months, :days), do: @days_in_month
  defp conversion_factor(:days, :hours), do: @hours_in_day
  defp conversion_factor(:hours, :minutes), do: @minutes_in_hour
  defp conversion_factor(:minutes, :seconds), do: @seconds_in_minute
  defp conversion_factor(:seconds, :seconds), do: 1

  defp lower_unit(:seconds), do: :seconds
  defp lower_unit(unit), do: Enum.at(@time_units, Enum.find_index(@time_units, &(&1 == unit)) + 1)

  defp duration_component(:years, duration), do: Timex.Duration.to_days(duration) / @days_in_year
  defp duration_component(:months, duration), do: Timex.Duration.to_days(duration) / @days_in_month
  defp duration_component(:days, duration), do: Timex.Duration.to_days(duration)
  defp duration_component(:hours, duration), do: Timex.Duration.to_hours(duration)
  defp duration_component(:minutes, duration), do: Timex.Duration.to_minutes(duration)
  defp duration_component(:seconds, duration), do: Timex.Duration.to_seconds(duration)

  defp snap(size, {x, y}) do
    left = floor_to(x, size / 2)

    if y <= left + size, do: {left, left + size}, else: nil
  end

  defp floor_to(x, grid), do: Float.floor(x / grid) * grid

  defp sizes(interval, size_factors) do
    Stream.concat(small_sizes(interval), large_sizes)
    |> Stream.flat_map(&(for factor <- size_factors, do: &1 * factor))
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
