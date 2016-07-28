defmodule Cloak.Query.Function do
  @functions %{
    ~w(count) => %{aggregate: true, extraction: false, type: :any},
    ~w(sum avg min max stddev median) => %{aggregate: true, extraction: false, type: :numeric},
    ~w(year month day hour minute second weekday) => %{aggregate: false, extraction: true, type: :timestamp},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  def function?({:function, _, _}), do: true
  def function?(_), do: false

  def valid_function?({:function, function, _}), do: Map.has_key?(@functions, function)

  def aggregate_function?({:function, function, _}), do: @functions[function].aggregate
  def aggregate_function?(_), do: false

  def argument_type({:function, function, _}), do: @functions[function].type

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
