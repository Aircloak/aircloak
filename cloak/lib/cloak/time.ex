defmodule Cloak.Time do
  @moduledoc "Contains utilities for normalizing date/times"

  @doc "Parses string as an ISO8601 time."
  @spec parse_time(String.t) :: {:ok, Calendar.time} | {:error, atom}
  def parse_time(string) do
    case Time.from_iso8601(string) do
      {:ok, result} -> {:ok, max_precision(result)}
      error -> error
    end
  end

  @doc "Parses string as an ISO8601 date with time. Will accept ISO date strings treating them as midnight."
  @spec parse_datetime(String.t) :: {:ok, NaiveDateTime.t} | {:error, atom}
  def parse_datetime(string) do
    case NaiveDateTime.from_iso8601(string) do
      {:ok, result} -> {:ok, max_precision(result)}
      _ -> case Timex.parse(string, "{ISOdate}") do
        {:ok, result} -> {:ok, max_precision(result)}
        error -> error
      end
    end
  end

  @doc "Parses string as an ISO8601 date with time."
  @spec parse_date(String.t) :: {:ok, Calendar.date} | {:error, atom}
  def parse_date(string), do: Date.from_iso8601(string)

  @doc "Sets the microsecond precision of the given Time or NaiveDateTime to 6."
  @spec max_precision(x) :: x when x: Time.t | NaiveDateTime.t
  def max_precision(datetime = %{microsecond: {usecs, _precision}}), do:
    %{datetime | microsecond: {usecs, 6}}

  @doc "Converts a date/datetime/time value into an integer representing days/seconds."
  @spec to_integer(NaiveDateTime.t | Time.t | Date.t) :: non_neg_integer
  def to_integer(%NaiveDateTime{} = value), do:
    value |> NaiveDateTime.to_erl() |> :calendar.datetime_to_gregorian_seconds()
  def to_integer(%Date{} = value), do:
    value |> Date.to_erl() |> :calendar.date_to_gregorian_days()
  def to_integer(%Time{} = value), do:
    value |> Time.to_erl() |> :calendar.time_to_seconds()

  @doc "Converts an integer representing days/seconds into a date/datetime/time value."
  @spec from_integer(non_neg_integer, :datetime | :date | :time) :: NaiveDateTime.t | Time.t | Date.t
  def from_integer(value, :datetime), do:
    value |> :calendar.gregorian_seconds_to_datetime() |> NaiveDateTime.from_erl!() |> Cloak.Time.max_precision()
  def from_integer(value, :date), do:
    value |> :calendar.gregorian_days_to_date() |> Date.from_erl!()
  def from_integer(value, :time), do:
    value |> :calendar.seconds_to_time() |> Time.from_erl!() |> max_precision()
end
