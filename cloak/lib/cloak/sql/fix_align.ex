defmodule Cloak.Sql.FixAlign do
  @moduledoc "Implements fixing the alignment of ranges to a predetermined grid."

  @type interval(x) :: {x, x}

  @default_size_factors [1, 2, 5]
  @midnight ~T[00:00:00]
  @just_before_midnight ~T[23:59:59.999999]
  @full_day {@midnight, @just_before_midnight}
  @time_units [:months, :days, :hours, :minutes, :seconds]
  @months_in_year 12
  @days_in_month 30
  @hours_in_day 24
  @minutes_in_hour 60
  @seconds_in_minute 60
  @seconds_in_day 24 * 60 * 60
  @seconds_in_hour 60 * 60
  @epsilon 1.0e-10

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
    Enum.min_by([baseline * 10, baseline * 5, baseline * 2, baseline], fn y -> abs(x - y) end)
  end

  def align(_), do: 0

  @doc """
  Returns an interval that has been aligned to a fixed grid. The density of the grid depends on the size of
  the input interval. Both ends of the input will be contained inside the output.
  """
  @spec align_interval(interval(x)) :: interval(x) when x: var
  def align_interval({x, y}) when is_boolean(x) and is_boolean(y), do: {x, y}

  def align_interval({x, y}) when is_number(x) and is_number(y) and x > y, do: raise("Invalid interval")

  def align_interval(interval = {x, y}) when is_number(x) and is_number(y), do: align_numeric_interval(interval)

  def align_interval({%NaiveDateTime{} = x, %NaiveDateTime{} = y}), do: align_datetime({x, y}) |> max_precision()

  def align_interval({%Date{} = x, %Date{} = y}), do: {x, y} |> align_datetime() |> to_date()

  def align_interval({%Time{} = x, %Time{} = y}),
    do:
      {x, y}
      |> time_to_datetime()
      |> align_datetime()
      |> datetime_to_time()
      |> cap_midnight()
      |> max_precision()

  # -------------------------------------------------------------------
  # Internal functions for Dates and NaiveDateTimes
  # -------------------------------------------------------------------

  defp time_to_datetime({x, y}) do
    if Cloak.Time.to_integer(x) < Cloak.Time.to_integer(y) do
      {time_to_datetime(x), time_to_datetime(y)}
    else
      raise "Invalid interval"
    end
  end

  defp time_to_datetime(%Time{hour: h, minute: m, second: s}),
    do: %NaiveDateTime{epoch_start() | hour: h, minute: m, second: s}

  defp datetime_to_time({x, y}) do
    if Timex.diff(x, y, :seconds) |> abs() >= @seconds_in_day do
      @full_day
    else
      {datetime_to_time(x), datetime_to_time(y)}
    end
  end

  defp datetime_to_time(%NaiveDateTime{hour: h, minute: m, second: s}), do: %Time{hour: h, minute: m, second: s}

  defp cap_midnight({x, y}) do
    if Cloak.Time.to_integer(x) > Cloak.Time.to_integer(y) do
      {x, @just_before_midnight}
    else
      {x, y}
    end
  end

  defp max_precision({x, y}), do: {Cloak.Time.max_precision(x), Cloak.Time.max_precision(y)}

  defp to_date({x, y}), do: {NaiveDateTime.to_date(x), NaiveDateTime.to_date(y)}

  defp align_datetime({x, y}) do
    x = apply_datetime_bounds(x)
    y = apply_datetime_bounds(y)

    if Timex.diff(y, x) <= 0 do
      raise "Invalid interval"
    else
      largest_unit = largest_changed_unit({x, y})
      aligned = align_datetime_once({x, y}, largest_unit)

      if largest_changed_unit(aligned) == largest_unit do
        aligned
      else
        align_datetime_once({x, y}, largest_changed_unit(aligned))
      end
    end
  end

  defp align_datetime_once(_interval, nil), do: raise("Invalid interval")

  defp align_datetime_once({x, y}, unit) do
    {x, y}
    |> units_since_epoch(unit)
    |> align_datetime_interval(unit, allow_half?(x, unit))
    |> datetime_from_units(unit)
  end

  defp largest_changed_unit({x, y}) do
    Enum.find(@time_units, fn component ->
      case Timex.diff(y, x, :duration) do
        %Timex.Duration{} = duration -> duration_component(component, duration) >= 1
        _ -> false
      end
    end)
  end

  defp align_datetime_interval(interval, unit, allow_half?) do
    unit
    |> datetime_sizes()
    |> Stream.map(&snap(&1, interval, allow_half?))
    |> Enum.find(& &1)
  end

  defp allow_half?(_, :months), do: false
  defp allow_half?(%Date{}, :days), do: false
  defp allow_half?(_, :seconds), do: false
  defp allow_half?(_, _), do: true

  defp datetime_sizes(:months), do: Stream.concat([1, 2], Stream.iterate(3, &(&1 + 3)))
  defp datetime_sizes(:days), do: [1, 2, 5, 10, 15, 20, 30, 60, 90]
  defp datetime_sizes(:hours), do: [1, 2, 6, 12, 24, 48]
  defp datetime_sizes(:minutes), do: [1, 2, 5, 15, 30, 60, 120]
  defp datetime_sizes(:seconds), do: [1, 2, 5, 15, 30, 60, 120]

  defp units_since_epoch({x, y}, unit),
    do: {units_since_epoch(x, unit), y |> datetime_ceil(lower_unit(unit)) |> units_since_epoch(unit)}

  defp units_since_epoch(datetime = %{year: year, month: month, day: day}, :months),
    do:
      (year - Cloak.Time.year_lower_bound()) * @months_in_year + (month - 1) +
        (day - 1) / Timex.days_in_month(datetime)

  defp units_since_epoch(datetime = %Date{}, :days),
    do: Timex.diff(datetime, epoch_start(), :days)

  defp units_since_epoch(datetime, :days),
    do: Timex.diff(datetime, epoch_start(), :seconds) / @seconds_in_day

  defp units_since_epoch(datetime, :hours),
    do: Timex.diff(datetime, epoch_start(), :seconds) / @seconds_in_hour

  defp units_since_epoch(datetime, :minutes),
    do: Timex.diff(datetime, epoch_start(), :seconds) / @seconds_in_minute

  defp units_since_epoch(datetime, :seconds),
    do: Timex.diff(datetime, epoch_start(), :seconds)

  defp datetime_ceil(datetime, :months) do
    if Timex.diff(datetime, Timex.beginning_of_month(datetime), :microseconds) == 0,
      do: datetime,
      else: Timex.beginning_of_month(datetime) |> Timex.shift(months: 1)
  end

  defp datetime_ceil(datetime, :days) do
    if Timex.diff(datetime, Timex.beginning_of_day(datetime), :microseconds) == 0,
      do: datetime,
      else: Timex.beginning_of_day(datetime) |> Timex.shift(days: 1)
  end

  defp datetime_ceil(datetime = %Date{}, :hours), do: datetime
  defp datetime_ceil(datetime = %{minute: 0, second: 0}, :hours), do: datetime

  defp datetime_ceil(datetime, :hours), do: %{datetime | hour: datetime.hour + 1, minute: 0, second: 0}

  defp datetime_ceil(datetime = %{second: 0}, :minutes), do: datetime
  defp datetime_ceil(datetime, :minutes), do: %{datetime | minute: datetime.minute + 1, second: 0}
  defp datetime_ceil(datetime, :seconds), do: datetime

  defp datetime_from_units({x, y}, unit), do: {datetime_from_units(x, unit), datetime_from_units(y, unit)}

  defp datetime_from_units(x, unit) do
    less_significant = ((x - Float.floor(x)) * conversion_factor(unit, lower_unit(unit))) |> round()

    more_significant = x |> Float.floor() |> round()

    epoch_start()
    |> Timex.shift([{unit, more_significant}, {lower_unit(unit), less_significant}])
    |> apply_datetime_bounds()
  end

  defp apply_datetime_bounds(datetime) do
    cond do
      datetime.year < Cloak.Time.year_lower_bound() -> epoch_start()
      datetime.year > Cloak.Time.year_upper_bound() -> epoch_end()
      true -> datetime
    end
  end

  defp conversion_factor(:months, :days), do: @days_in_month
  defp conversion_factor(:days, :hours), do: @hours_in_day
  defp conversion_factor(:hours, :minutes), do: @minutes_in_hour
  defp conversion_factor(:minutes, :seconds), do: @seconds_in_minute
  defp conversion_factor(:seconds, :seconds), do: 1

  defp lower_unit(:seconds), do: :seconds
  defp lower_unit(unit), do: Enum.at(@time_units, Enum.find_index(@time_units, &(&1 == unit)) + 1)

  defp duration_component(:months, duration), do: Timex.Duration.to_days(duration) / @days_in_month
  defp duration_component(:days, duration), do: Timex.Duration.to_days(duration)
  defp duration_component(:hours, duration), do: Timex.Duration.to_hours(duration)
  defp duration_component(:minutes, duration), do: Timex.Duration.to_minutes(duration)
  defp duration_component(:seconds, duration), do: Timex.Duration.to_seconds(duration)

  # -------------------------------------------------------------------
  # Internal functions for numeric intervals
  # -------------------------------------------------------------------

  defp align_numeric_interval(interval) do
    interval
    |> numeric_sizes()
    |> Stream.map(&snap(&1, interval, _allow_half? = true))
    |> Enum.find(& &1)
  end

  defp snap(size, {x, y}, allow_half?) do
    require Integer

    can_halve? = allow_half? or (is_integer(size) and Integer.is_even(size))
    left = floor_to(x, size)
    left = if can_halve?, do: floor_to(x, size / 2) |> max(left), else: left
    right = left + size

    if y <= right * (1 + sign(right) * @epsilon), do: {left, right}, else: nil
  end

  defp sign(x) when x < 0, do: -1
  defp sign(_), do: 1

  defp floor_to(x, grid) do
    floor_epsilon(x / grid) * grid
  end

  defp floor_epsilon(x) do
    if abs(Float.round(x) - x) < @epsilon do
      Float.round(x)
    else
      Float.floor(x)
    end
  end

  defp numeric_sizes(interval) do
    Stream.concat(small_sizes(interval), large_sizes())
    |> Stream.flat_map(fn magnitude -> Enum.map(@default_size_factors, &(&1 * magnitude)) end)
  end

  defp small_sizes({x, y}) do
    start =
      ((y - x) / 2)
      |> :math.log10()
      |> Float.floor()
      |> round()

    if start < 0, do: Stream.map(start..-1, &:math.pow(10, &1)), else: []
  end

  defp large_sizes(), do: Stream.iterate(1, &(&1 * 10))

  defp order_of_magnitude(x) do
    floor_log = x |> :math.log10() |> Float.floor()
    :math.pow(10, floor_log)
  end

  defp epoch_start(), do: NaiveDateTime.from_erl!({{Cloak.Time.year_lower_bound(), 1, 1}, {0, 0, 0}})
  defp epoch_end(), do: NaiveDateTime.from_erl!({{Cloak.Time.year_upper_bound(), 12, 31}, {23, 59, 59}}, {999_999, 6})
end
