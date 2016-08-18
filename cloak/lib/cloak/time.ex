defmodule Cloak.Time do
  def parse_time(string) do
    case Time.from_iso8601(string) do
      {:ok, result} -> {:ok, max_precision(result)}
      error -> error
    end
  end

  def parse_datetime(string) do
    case NaiveDateTime.from_iso8601(string) do
      {:ok, result} -> {:ok, max_precision(result)}
      error -> error
    end
  end

  def parse_date(string), do: Date.from_iso8601(string)

  def max_precision(datetime = %{microsecond: {usecs, _precision}}), do:
    %{datetime | microsecond: {usecs, 6}}
end
