defmodule Cloak.Query.Function do
  def apply(value, "year"), do: value.year
  def apply(value, "month"), do: value.month
  def apply(value, "day"), do: value.day
  def apply(value, "hour"), do: value.hour
  def apply(value, "minute"), do: value.minute
  def apply(value, "second"), do: value.second
  def apply(value, "weekday"), do: Timex.weekday(value)
  def apply(value, _), do: value
end
