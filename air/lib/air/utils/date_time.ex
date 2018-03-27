defmodule Air.Utils.DateTime do
  @moduledoc """
  Utilities for working with datetimes
  """

  @doc """
  Converts an ecto datetime into a human readable text representation of how long in the past the
  datetime lies. For example, "1 second ago", "4 days ago", etc.
  """
  @spec time_ago(Calendar.date_time()) :: String.t()
  def time_ago(date_time), do: Timex.from_now(date_time)
end
