defmodule Cloak.Sql.FixAlign do
  @moduledoc "Implements fixing the alignment of ranges to a predetermined grid."

  @type interval(x) :: {x, x}

  @default_size_factors [1, 2, 5]
  @epoch ~N[1970-01-01 00:00:00]
  @min_date ~N[1583-01-01 00:00:00]
  @max_date ~N[9999-12-31 23:59:59.999999]
  @midnight ~T[00:00:00]
  @just_before_midnight ~T[23:59:59.999999]
  @full_day {@midnight, @just_before_midnight}
  @time_units [:years, :months, :days, :hours, :minutes, :seconds]
  @months_in_year 12
  @days_in_year 365
  @days_in_month 30
  @hours_in_day 24
  @minutes_in_hour 60
  @seconds_in_minute 60
  @seconds_in_day 24 * 60 * 60
  @seconds_in_hour 60 * 60


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

  Options:
  :allow_fractions - allow intervals smaller than 1 unit, defaults to true
  :allow_half - allow intervals to be aligned by half their size (for example {2.5, 7.5}), defaults to true
  :size_factors - the sizes of intervals to use, defaults to [1, 2, 5] (money-aligned), will be extended both ways
    (for example to [..., 0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, ...])
  """
  @spec align_interval(interval(x), Keyword.t) :: interval(x) when x: var
  def align_interval(interval, opts \\ [])
  def align_interval({x, y}, _) when is_number(x) and is_number(y) and x > y, do: raise "Invalid interval"
  def align_interval(interval = {x, y}, opts) when is_number(x) and is_number(y) do
    allow_fractions = Keyword.get(opts, :allow_fractions, true)
    allow_half = Keyword.get(opts, :allow_half, true)
    size_factors = Keyword.get(opts, :size_factors, @default_size_factors)

    interval
    |> sizes(size_factors, allow_fractions)
    |> Stream.map(&snap(&1, interval, allow_half))
    |> Enum.find(&(&1))
  end
  def align_interval({%NaiveDateTime{} = x, %NaiveDateTime{} = y}, _), do: align_date_time({x, y}) |> max_precision()
  def align_interval({%Date{} = x, %Date{} = y}, _), do: {x, y} |> align_date_time() |> to_date()
  def align_interval({%Time{} = x, %Time{} = y}, _), do:
    {x, y} |> time_to_datetime() |> align_date_time() |> datetime_to_time() |> cap_midnight() |> max_precision()


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
  defp time_to_datetime(%Time{hour: h, minute: m, second: s}), do: %{@epoch | hour: h, minute: m, second: s}

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

  defp align_date_time({x, y}) do
    if Timex.diff(y, x) <= 0 do
      raise "Invalid interval"
    else
      aligned = align_date_time_once({x, y}, largest_changed_unit({x, y}))
      if largest_changed_unit(aligned) == largest_changed_unit({x, y}) do
        aligned
      else
        align_date_time_once({x, y}, largest_changed_unit(aligned))
      end
    end
  end

  defp align_date_time_once(_interval, nil), do: raise "Invalid interval"
  defp align_date_time_once({x, y}, unit) do
    {x, y}
    |> units_since_epoch(unit)
    |> align_interval(size_factors: size_factors(unit), allow_fractions: false, allow_half: allow_half?(x, unit))
    |> datetime_from_units(unit)
  end

  # because of the timex bug (https://github.com/bitwalker/timex/pull/239)
  @dialyzer {:no_match, largest_changed_unit: 1}
  defp largest_changed_unit({x, y}) do
    Enum.find(@time_units, fn(component) ->
      case Timex.diff(y, x, :duration) do
        %Timex.Duration{} = duration -> duration_component(component, duration) >= 1
        _ -> false
      end
    end)
  end

  defp allow_half?(_, :months), do: false
  defp allow_half?(%Date{}, :days), do: false
  defp allow_half?(_, :seconds), do: false
  defp allow_half?(_, _), do: true

  defp size_factors(:years), do: @default_size_factors
  defp size_factors(:months), do: [1, 2, 6]
  defp size_factors(:days), do: @default_size_factors
  defp size_factors(:hours), do: [1, 2, 6, 12, 24]
  defp size_factors(:minutes), do: [1, 2, 5, 15, 30, 60]
  defp size_factors(:seconds), do: [1, 2, 5, 15, 30, 60]

  defp units_since_epoch({x, y}, unit), do:
    {units_since_epoch(x, unit), y |> datetime_ceil(lower_unit(unit)) |> units_since_epoch(unit)}
  defp units_since_epoch(%{year: year, month: month}, :years), do:
    year - @epoch.year + (month - @epoch.month) / @months_in_year
  defp units_since_epoch(datetime = %{year: year, month: month, day: day}, :months), do:
    (year - @epoch.year) * @months_in_year + (month - @epoch.month) + (day - @epoch.day) / Timex.days_in_month(datetime)
  defp units_since_epoch(datetime = %Date{}, :days), do: Timex.diff(datetime, @epoch, :days)
  defp units_since_epoch(datetime, :days), do:
    Timex.diff(datetime, @epoch, :seconds) / @seconds_in_day
  defp units_since_epoch(datetime, :hours), do:
    Timex.diff(datetime, @epoch, :seconds) / @seconds_in_hour
  defp units_since_epoch(datetime, :minutes), do:
    Timex.diff(datetime, @epoch, :seconds) / @seconds_in_minute
  defp units_since_epoch(datetime, :seconds), do: Timex.diff(datetime, @epoch, :seconds)

  defp datetime_ceil(datetime, :months), do:
    if Timex.diff(datetime, Timex.beginning_of_month(datetime)) == 0,
      do: datetime,
      else: Timex.beginning_of_month(datetime) |> shift(months: 1)
  defp datetime_ceil(datetime, :days), do:
    if Timex.diff(datetime, Timex.beginning_of_day(datetime)) == 0,
      do: datetime,
      else: Timex.beginning_of_day(datetime) |> shift(days: 1)
  defp datetime_ceil(datetime = %Date{}, :hours), do: datetime
  defp datetime_ceil(datetime = %{minute: 0, second: 0}, :hours), do: datetime
  defp datetime_ceil(datetime, :hours), do: %{datetime | hour: datetime.hour + 1, minute: 0, second: 0}
  defp datetime_ceil(datetime = %{second: 0}, :minutes), do: datetime
  defp datetime_ceil(datetime, :minutes), do: %{datetime | minute: datetime.minute + 1, second: 0}
  defp datetime_ceil(datetime, :seconds), do: datetime

  # Workaround for https://github.com/bitwalker/timex/pull/235
  defp shift(datetime, spec), do: Timex.Protocol.shift(datetime, spec)

  defp datetime_from_units({x, y}, unit), do: {datetime_from_units(x, unit), datetime_from_units(y, unit)}
  defp datetime_from_units(x, unit) do
    cond do
      unit == :years and @epoch.year + x < @min_date.year -> @min_date
      unit == :years and @epoch.year + x > @max_date.year -> @max_date
      true ->
        less_significant = (x - Float.floor(x)) * conversion_factor(unit, lower_unit(unit)) |> round()
        more_significant = x |> Float.floor() |> round()
        Timex.shift(@epoch, [{unit, more_significant}, {lower_unit(unit), less_significant}])
    end
  end

  defp conversion_factor(:years, :months), do: @months_in_year
  defp conversion_factor(:months, :days), do: @days_in_month
  defp conversion_factor(:days, :hours), do: @hours_in_day
  defp conversion_factor(:hours, :minutes), do: @minutes_in_hour
  defp conversion_factor(:minutes, :seconds), do: @seconds_in_minute
  defp conversion_factor(:seconds, :seconds), do: 1

  defp lower_unit(:seconds), do: :seconds
  defp lower_unit(unit), do: Enum.at(@time_units, Enum.find_index(@time_units, &(&1 == unit)) + 1)

  # because of the timex bug (https://github.com/bitwalker/timex/pull/239)
  @dialyzer {:no_unused, duration_component: 2}
  defp duration_component(:years, duration), do: Timex.Duration.to_days(duration) / @days_in_year
  defp duration_component(:months, duration), do: Timex.Duration.to_days(duration) / @days_in_month
  defp duration_component(:days, duration), do: Timex.Duration.to_days(duration)
  defp duration_component(:hours, duration), do: Timex.Duration.to_hours(duration)
  defp duration_component(:minutes, duration), do: Timex.Duration.to_minutes(duration)
  defp duration_component(:seconds, duration), do: Timex.Duration.to_seconds(duration)


  # -------------------------------------------------------------------
  # Internal functions for numeric intervals
  # -------------------------------------------------------------------

  defp snap(size, {x, y}, allow_half) do
    require Integer

    can_halve = allow_half || (is_integer(size) && Integer.is_even(size))
    left = if can_halve, do: floor_to(x, size / 2), else: floor_to(x, size)

    if y <= left + size, do: {left, left + size}, else: nil
  end

  defp floor_to(x, grid), do: Float.floor(x / grid) * grid

  defp sizes(interval, size_factors, allow_fractions) do
    Stream.concat(small_sizes(interval, allow_fractions), large_sizes())
    |> Stream.flat_map(&(for factor <- size_factors, do: &1 * factor))
  end

  defp small_sizes(_, _allow_fractions = false), do: []
  defp small_sizes({x, y}, _allow_fractions = true) do
    start =
      (y - x) / 2
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
