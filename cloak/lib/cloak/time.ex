defmodule Cloak.Time do
  @moduledoc "Contains utilities for normalizing date/times"

  @doc "Parses string as an ISO8601 time."
  @spec parse_time(String.t) :: {:ok, Time.t} | {:error, atom}
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
  @spec parse_date(String.t) :: {:ok, Date.t} | {:error, atom}
  def parse_date(string), do: Date.from_iso8601(string)

  @doc "Sets the microsecond precision of the given Time or NaiveDateTime to 6."
  @spec max_precision(x) :: x when x: Time.t | NaiveDateTime.t
  def max_precision(datetime = %{microsecond: {usecs, _precision}}), do:
    %{datetime | microsecond: {usecs, 6}}
end
