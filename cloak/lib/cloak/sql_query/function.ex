defmodule Cloak.SqlQuery.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.SqlQuery.{Column, Parser}
  alias Cloak.DataSource

  @functions %{
    ~w(count) => %{aggregate: true, argument_types: [:any]},
    ~w(sum avg min max stddev median) => %{aggregate: true, argument_types: [:numeric]},
    ~w(year month day hour minute second weekday) => %{aggregate: false, argument_types: [:timestamp]},
    ~w(floor ceil ceiling) => %{aggregate: false, argument_types: [:real]},
    ~w(round trunc) => %{aggregate: false, argument_types: [:real, {:optional, :integer}]},
    ~w(abs sqrt) => %{aggregate: false, argument_types: [:numeric]},
    ~w(div mod) => %{aggregate: false, argument_types: [:integer, :integer]},
    ~w(pow) => %{aggregate: false, argument_types: [:numeric, :numeric]},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  @type column :: Parser.column | Column.t
  @type data_type :: :any | :numeric | :timestamp | DataSource.data_type
  @type argument_type :: data_type | {:optional, data_type}


  # -------------------------------------------------------------------
  # Info functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column definition is a function call, false otherwise."
  @spec function?(column) :: boolean
  def function?({:function, _, _}), do: true
  def function?(_), do: false

  @doc "Returns true if the given function call to a known function, false otherwise."
  @spec valid_function?(column) :: boolean
  def valid_function?({:function, function, _}), do: Map.has_key?(@functions, function)

  @doc """
  Returns true if the given column definition is a function call to an aggregate function, false otherwise.
  """
  @spec aggregate_function?(column) :: boolean
  def aggregate_function?({:function, function, _}), do: @functions[function].aggregate
  def aggregate_function?(_), do: false

  @doc "Returns the list of argument types required by the given function call."
  @spec argument_types(column) :: [argument_type]
  def argument_types({:function, function, _}), do: @functions[function].argument_types

  @doc "Returns the argument specifiaction of the given function call."
  @spec arguments(column) :: [Column.t]
  def arguments({:function, _, arguments}), do: arguments

  @doc "Returns the function name of the given function call."
  @spec name(column) :: String.t
  def name({:function, name, _}), do: name

  @doc "Returns true if the arguments to the given function call match the expected argument types, false otherwise."
  @spec well_typed?(column) :: boolean
  def well_typed?(function) do
    length(arguments(function)) <= length(argument_types(function)) &&
      argument_types(function)
      |> Enum.with_index()
      |> Enum.all?(fn({type, index}) -> type_matches?(type, Enum.at(arguments(function), index)) end)
  end


  # -------------------------------------------------------------------
  # Implementations
  # -------------------------------------------------------------------

  @doc "Returns the result of applying the given function definition to the given value."
  @spec apply(term, column) :: term
  def apply(args = [_|_], function), do:
    if Enum.member?(args, :*), do: :*, else: do_apply(args, function)
  def apply(value, _aggregate_function), do: value


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp type_matches?({:optional, _}, nil), do: true
  defp type_matches?(_, nil), do: false
  defp type_matches?({:optional, type}, argument), do: type_matches?(type, argument)
  defp type_matches?(:any, _), do: true
  defp type_matches?(expected_type, %{type: actual_type}) do
    case {expected_type, actual_type} do
      {:numeric, :integer} -> true
      {:numeric, :real} -> true
      {type, type} -> true
      _ -> false
    end
  end

  defp do_apply([value], {:function, "year", _}), do: value.year
  defp do_apply([value], {:function, "month", _}), do: value.month
  defp do_apply([value], {:function, "day", _}), do: value.day
  defp do_apply([value], {:function, "hour", _}), do: value.hour
  defp do_apply([value], {:function, "minute", _}), do: value.minute
  defp do_apply([value], {:function, "second", _}), do: value.second
  defp do_apply([value], {:function, "weekday", _}), do: Timex.weekday(value)
  defp do_apply([value], {:function, "sqrt", _}), do: :math.sqrt(value)
  defp do_apply([value], {:function, "floor", _}), do: Float.floor(value)
  defp do_apply([value], {:function, "ceil", _}), do: Float.ceil(value)
  defp do_apply([value], {:function, "ceiling", _}), do: Float.ceil(value)
  defp do_apply([value], {:function, "abs", _}), do: abs(value)
  defp do_apply([value], {:function, "round", _}), do: round(value)
  defp do_apply([value, precision], {:function, "round", _}), do: Float.round(value, precision)
  defp do_apply([value], {:function, "trunc", _}), do: trunc(value)
  defp do_apply([value, precision], {:function, "trunc", _}), do: do_trunc(value, precision)
  defp do_apply([x, y], {:function, "div", _}), do: div(x, y)
  defp do_apply([x, y], {:function, "mod", _}), do: rem(x, y)
  defp do_apply([x, y], {:function, "pow", _}), do: :math.pow(x, y)

  defp do_trunc(value, 0), do: trunc(value)
  defp do_trunc(value, precision) when value < 0, do: Float.ceil(value, precision)
  defp do_trunc(value, precision), do: Float.floor(value, precision)
end
