defmodule Cloak.SqlQuery.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.SqlQuery.Parser

  @functions %{
    ~w(count) => %{aggregate: true, extraction: false, type: :any},
    ~w(sum avg min max stddev median) => %{aggregate: true, extraction: false, type: :numeric},
    ~w(year month day hour minute second weekday) => %{aggregate: false, extraction: true, type: :timestamp},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  @type argument_type :: :any | :numeric | :timestamp


  # -------------------------------------------------------------------
  # Info functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column definition is a function call, false otherwise."
  @spec function?(Parser.column | Cloak.SqlQuery.Column.t) :: boolean
  def function?({:function, _, _}), do: true
  def function?(_), do: false

  @doc "Returns true if the given function call to a known function, false otherwise."
  @spec valid_function?(Parser.column | Cloak.SqlQuery.Column.t) :: boolean
  def valid_function?({:function, function, _}), do: Map.has_key?(@functions, function)

  @doc """
  Returns true if the given column definition is a function call to an aggregate function, false otherwise.
  """
  @spec aggregate_function?(Parser.column | Cloak.SqlQuery.Column.t) :: boolean
  def aggregate_function?({:function, function, _}), do: @functions[function].aggregate
  def aggregate_function?(_), do: false

  @doc "Returns the argument type required by the given function call."
  @spec argument_type(Parser.column | Cloak.SqlQuery.Column.t) :: argument_type
  def argument_type({:function, function, _}), do: @functions[function].type

  @doc "Returns the argument specifiaction of the given function call."
  @spec argument(Parser.column | Cloak.SqlQuery.Column.t) :: Cloak.SqlQuery.Column.t
  def argument({:function, _, argument}), do: argument

  @doc "Returns the function name of the given function call."
  @spec name(Parser.column) :: String.t
  def name({:function, name, _}), do: name

  # -------------------------------------------------------------------
  # Implementations
  # -------------------------------------------------------------------

  @doc "Returns the result of applying the given function definition to the given value."
  @spec apply(term, Parser.column | Cloak.SqlQuery.Column.t) :: term
  def apply(:*, _), do: :*
  def apply(value, {:function, "year", _}), do: value.year
  def apply(value, {:function, "month", _}), do: value.month
  def apply(value, {:function, "day", _}), do: value.day
  def apply(value, {:function, "hour", _}), do: value.hour
  def apply(value, {:function, "minute", _}), do: value.minute
  def apply(value, {:function, "second", _}), do: value.second
  def apply(value, {:function, "weekday", _}), do: Timex.weekday(value)
  def apply(value, _), do: value
end
