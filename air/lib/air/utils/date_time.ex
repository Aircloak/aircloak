defmodule Air.Utils.DateTime do
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
    |> NaiveDateTime.from_erl!()
    |> Timex.from_now()
  end
end
