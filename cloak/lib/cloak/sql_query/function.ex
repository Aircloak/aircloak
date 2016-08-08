defmodule Cloak.SqlQuery.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.SqlQuery.{Column, Parser}
  alias Cloak.DataSource

  import Kernel, except: [apply: 2]

  @functions %{
    ~w(count) => %{aggregate: true, return_type: :integer, argument_types: [:any]},
    ~w(sum avg min max stddev median) => %{aggregate: true, return_type: :real, argument_types: [:numeric]},
    ~w(year month day hour minute second weekday) =>
      %{aggregate: false, return_type: :integer, argument_types: [:timestamp]},
    ~w(floor ceil ceiling) => %{aggregate: false, return_type: :real, argument_types: [:numeric]},
    ~w(round trunc) => %{aggregate: false, return_type: :real, argument_types: [:numeric, {:optional, :integer}]},
    ~w(abs sqrt) => %{aggregate: false, return_type: :real, argument_types: [:numeric]},
    ~w(div mod) => %{aggregate: false, return_type: :integer, argument_types: [:integer, :integer]},
    ~w(pow) => %{aggregate: false, return_type: :real, argument_types: [:numeric, :numeric]},
    ~w(length) => %{aggregate: false, return_type: :integer, argument_types: [:text]},
    ~w(lower lcase upper ucase) => %{aggregate: false, return_type: :text, argument_types: [:text]},
    ~w(left right) => %{aggregate: false, return_type: :text, argument_types: [:text, :integer]},
    ~w(btrim ltrim rtrim) => %{aggregate: false, return_type: :text, argument_types: [:text, {:optional, :text}]},
    ~w(substring substring_for) =>
      %{aggregate: false, return_type: :text, argument_types: [:text, :integer, {:optional, :integer}]},
    ~w(concat) => %{aggregate: false, return_type: :text, argument_types: [{:many1, :text}]},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  @type t :: Parser.column | Column.t
  @type data_type :: :any | :numeric | :timestamp | DataSource.data_type
  @type argument_type :: data_type | {:optional, data_type} | {:many1, data_type}


  # -------------------------------------------------------------------
  # Info functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column definition is a function call, false otherwise."
  @spec function?(t) :: boolean
  def function?({:function, _, _}), do: true
  def function?(_), do: false

  @doc "Returns true if the given function call to a known function, false otherwise."
  @spec valid_function?(t) :: boolean
  def valid_function?({:function, function, _}), do: Map.has_key?(@functions, function)

  @doc """
  Returns true if the given column definition is a function call to an aggregate function, false otherwise.
  """
  @spec aggregate_function?(t) :: boolean
  def aggregate_function?({:function, function, _}), do: @functions[function].aggregate
  def aggregate_function?(_), do: false

  @doc "Returns the argument type required by the given function call."
  @spec argument_types(t) :: [argument_type]
  def argument_types({:function, function, _}), do: @functions[function].argument_types

  @doc "Returns the argument specifiaction of the given function call."
  @spec arguments(t) :: [Column.t]
  def arguments({:function, _, arguments}), do: arguments
  def arguments(_), do: []

  @doc "Returns the function name of the given function call."
  @spec name(t) :: String.t
  def name({:function, name, _}), do: name

  @doc "Returns the return type of the given function call."
  @spec return_type(t) :: data_type
  def return_type({:function, name, _}), do: @functions[name].return_type

  @doc "Returns the type of the given expression."
  @spec type(t) :: data_type
  def type(function = {:function, _, _}), do: return_type(function)
  def type(%Column{type: type}), do: type

  @doc "Returns true if the arguments to the given function call match the expected argument types, false otherwise."
  @spec well_typed?(t) :: boolean
  def well_typed?(column), do:
    if function?(column),
      do: do_well_typed?(column, argument_types(column)),
      else: true

  @doc "Applies the function to the database row and returns its result."
  @spec apply_to_db_row(t, DataSource.row) :: term
  def apply_to_db_row(%Column{} = column, row),
    do: Column.value(column, row)
  def apply_to_db_row(function, row) do
    true = function?(function)

    function
    |> arguments()
    |> Enum.map(&Column.value(&1, row))
    |> apply(function)
  end

  @doc "Returns the result of applying the given function definition to the given value."
  @spec apply([term], t) :: term
  def apply(args = [_|_], function), do:
    if Enum.member?(args, :*), do: :*, else: do_apply(args, function)
  def apply(value, _aggregate_function), do: value


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_well_typed?(function, [{:many1, type}]), do:
    Enum.all?(arguments(function), &type_matches?(type, &1))
  defp do_well_typed?(function, argument_types) do
    length(arguments(function)) <= length(argument_types) &&
      argument_types
      |> Enum.with_index()
      |> Enum.all?(fn({type, index}) -> type_matches?(type, Enum.at(arguments(function), index)) end)
  end

  defp type_matches?(type, function = {:function, _, _}), do:
    type_matches?(type, %{type: return_type(function)})
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
  defp do_apply([value], {:function, "floor", _}), do: value |> :erlang.float() |> Float.floor()
  defp do_apply([value], {:function, "ceil", _}), do: value |> :erlang.float() |> Float.ceil()
  defp do_apply([value], {:function, "ceiling", _}), do: value |> :erlang.float() |> Float.ceil()
  defp do_apply([value], {:function, "abs", _}), do: abs(value)
  defp do_apply([value], {:function, "round", _}), do: round(value)
  defp do_apply([value, precision], {:function, "round", _}), do: value |> :erlang.float() |> Float.round(precision)
  defp do_apply([value], {:function, "trunc", _}), do: trunc(value)
  defp do_apply([value, precision], {:function, "trunc", _}), do: do_trunc(value, precision)
  defp do_apply([x, y], {:function, "div", _}), do: div(x, y)
  defp do_apply([x, y], {:function, "mod", _}), do: rem(x, y)
  defp do_apply([x, y], {:function, "pow", _}), do: :math.pow(x, y)
  defp do_apply([string], {:function, "length", _}), do: String.length(string)
  defp do_apply([string], {:function, "lower", _}), do: String.downcase(string)
  defp do_apply([string], {:function, "lcase", _}), do: String.downcase(string)
  defp do_apply([string], {:function, "upper", _}), do: String.upcase(string)
  defp do_apply([string], {:function, "ucase", _}), do: String.upcase(string)
  defp do_apply([string], {:function, "btrim", _}), do: trim(string, " ")
  defp do_apply([string, chars], {:function, "btrim", _}), do: trim(string, chars)
  defp do_apply([string], {:function, "ltrim", _}), do: ltrim(string, " ")
  defp do_apply([string, chars], {:function, "ltrim", _}), do: ltrim(string, chars)
  defp do_apply([string], {:function, "rtrim", _}), do: rtrim(string, " ")
  defp do_apply([string, chars], {:function, "rtrim", _}), do: rtrim(string, chars)
  defp do_apply([string, count], {:function, "left", _}), do: left(string, count)
  defp do_apply([string, count], {:function, "right", _}), do: right(string, count)
  defp do_apply([string, from], {:function, "substring", _}), do: substring(string, from)
  defp do_apply([string, from, count], {:function, "substring", _}), do: substring(string, from, count)
  defp do_apply([string, count], {:function, "substring_for", _}), do: substring(string, 1, count)
  defp do_apply(args, {:function, "concat", _}), do: Enum.join(args)

  defp do_trunc(value, 0), do: trunc(value)
  defp do_trunc(value, precision) when value < 0, do: value |> :erlang.float() |> Float.ceil(precision)
  defp do_trunc(value, precision), do: value |> :erlang.float() |> Float.floor(precision)

  defp left(string, count) when count < 0, do:
    String.slice(string, 0, max(String.length(string) + count, 0))
  defp left(string, count), do: String.slice(string, 0, count)

  defp right(string, count) when count < 0, do: String.slice(string, -count, String.length(string))
  defp right(string, count), do: String.slice(string, String.length(string) - count, count)

  defp trim(string, chars), do: string |> ltrim(chars) |> rtrim(chars)

  defp ltrim(string, chars), do: Regex.replace(~r/^[#{Regex.escape(chars)}]*/, string, "")

  defp rtrim(string, chars), do: Regex.replace(~r/[#{Regex.escape(chars)}]*$/, string, "")

  defp substring(string, from, count \\ nil)
  defp substring(string, from, count) when from < 1, do: substring(string, 1, count + from - 1)
  defp substring(_string, _from, count) when count < 0, do: ""
  defp substring(string, from, count), do:
    String.slice(string, from - 1, count || String.length(string))
end
