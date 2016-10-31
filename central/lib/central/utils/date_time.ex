defmodule Central.Utils.DateTime do
  @moduledoc """
  Utilities for working with datetimes
  """

  @doc """
  Converts an ecto datetime into a human readable text representation of how long in the past the
  datetime lies. For example, "1 second ago", "4 days ago", etc.
  """
  @spec time_ago(Ecto.DateTime.t) :: String.t
  def time_ago(date_time) do
    date_time
    |> Ecto.DateTime.to_erl()
    |> Timex.DateTime.from_erl()
    |> Timex.from_now()
  end

  @doc """
  Returns an Ecto.DateTime dated N days in the past.

  Please note that the hour, minutes and seconds are all set to 0.
  Therefore if you want to use the returned value in a database compairson,
  and want all records for the current day included, then you should use
  -1 as your parameter, or alternative use `datetime_days_in_the_future with 1
  as the parameter in order to get a comparison like this:

  ... where: inserted_at <= <tomorrow's date at midnight>
  """
  @spec datetime_days_ago(integer) :: Ecto.DateTime.t
  def datetime_days_ago(days) do
    interval = Timex.Time.to_timestamp(days, :days)
    now = Timex.Date.now()
    Timex.subtract(now, interval)
    |> Timex.to_erlang_datetime()
    |> Ecto.DateTime.from_erl()
  end

  @doc """
  Returns an Ecto.DateTime dated N days in the future.
  The results are the same as you would get when calling datetime_days_ago
  with the parameter negated.
  """
  @spec datetime_days_in_the_future(integer) :: Ecto.DateTime.t
  def datetime_days_in_the_future(days), do: datetime_days_ago(-1 * days)
end
