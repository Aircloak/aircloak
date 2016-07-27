defmodule Cloak.Query.Function do
  def extraction_function?({:function, name, _}) do
    Enum.member?(~w(year month day hour minute second weekday), name)
  end
  def extraction_function?(_), do: false

  def argument({:function, _, argument}), do: argument

  def apply(value, {:function, "year", _}), do: value.year
  def apply(value, {:function, "month", _}), do: value.month
  def apply(value, {:function, "day", _}), do: value.day
  def apply(value, {:function, "hour", _}), do: value.hour
  def apply(value, {:function, "minute", _}), do: value.minute
  def apply(value, {:function, "second", _}), do: value.second
  def apply(value, {:function, "weekday", _}), do: Timex.weekday(value)
  def apply(value, _), do: value
end
