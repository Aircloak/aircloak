defmodule Cloak.Aql.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.Aql.{Column, Parser}
  alias Cloak.DataSource

  import Kernel, except: [apply: 2]

  numeric = {:or, [:integer, :real]}
  @functions %{
    ~w(count) => %{aggregate: true, return_type: :integer, argument_types: [:any]},
    ~w(sum avg min max stddev median) => %{aggregate: true, return_type: :real, argument_types: [numeric]},
    ~w(hour minute second) =>
      %{aggregate: false, return_type: :integer, argument_types: [{:or, [:timestamp, :time]}]},
    ~w(year month day weekday) =>
      %{aggregate: false, return_type: :integer, argument_types: [{:or, [:timestamp, :date]}]},
    ~w(floor ceil ceiling) => %{aggregate: false, return_type: :integer, argument_types: [numeric]},
    ~w(round trunc) => %{aggregate: false, return_type: :real, argument_types: [numeric, {:optional, :integer}]},
    ~w(abs sqrt) => %{aggregate: false, return_type: :real, argument_types: [numeric]},
    ~w(div mod %) => %{aggregate: false, return_type: :integer, argument_types: [:integer, :integer]},
    ~w(pow * + - / ^) => %{aggregate: false, return_type: :real, argument_types: [numeric, numeric]},
    ~w(length) => %{aggregate: false, return_type: :integer, argument_types: [:text]},
    ~w(lower lcase upper ucase) => %{aggregate: false, return_type: :text, argument_types: [:text]},
    ~w(left right) => %{aggregate: false, return_type: :text, argument_types: [:text, :integer]},
    ~w(btrim ltrim rtrim) => %{aggregate: false, return_type: :text, argument_types: [:text, {:optional, :text}]},
    ~w(substring substring_for) =>
      %{aggregate: false, return_type: :text, argument_types: [:text, :integer, {:optional, :integer}]},
    ~w(||) => %{aggregate: false, return_type: :text, argument_types: [:text, :text]},
    ~w(concat) => %{aggregate: false, return_type: :text, argument_types: [{:many1, :text}]},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})
  |> Map.merge(%{
    {"cast", :integer} =>
      %{aggregate: false, return_type: :integer, argument_types: [{:or, [:real, :integer, :text, :boolean]}]},
    {"cast", :date} => %{aggregate: false, return_type: :date, argument_types: [:any]}
  })

  @type t :: Parser.column | Column.t
  @type data_type :: :any | DataSource.data_type
  @type argument_type :: data_type | {:optional, data_type} | {:many1, data_type} | {:or, [data_type]}


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

  @doc "Returns true if the given function call is a cast, false otherwise."
  @spec cast?(t) :: boolean
  def cast?({:function, {"cast", _}, _}), do: true
  def cast?(_), do: false

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
    |> Enum.map(&apply_to_db_row(&1, row))
    |> apply(function)
  end

  @doc "Returns the result of applying the given function definition to the given value."
  @spec apply([term], t) :: term
  def apply(args = [_|_], {:function, name, _}), do:
    if Enum.member?(args, :*), do: :*, else: do_apply(name, args)
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
  defp type_matches?({:or, types}, argument), do: Enum.any?(types, &type_matches?(&1, argument))
  defp type_matches?(:any, _), do: true
  defp type_matches?(expected_type, %{type: actual_type}), do:
    expected_type == actual_type

  defp do_apply("year", [value]), do: value.year
  defp do_apply("month", [value]), do: value.month
  defp do_apply("day", [value]), do: value.day
  defp do_apply("hour", [value]), do: value.hour
  defp do_apply("minute", [value]), do: value.minute
  defp do_apply("second", [value]), do: value.second
  defp do_apply("weekday", [value]), do: Timex.weekday(value)
  defp do_apply("sqrt", [value]), do: :math.sqrt(value)
  defp do_apply("floor", [value]) when is_integer(value), do: value
  defp do_apply("floor", [value]), do: value |> Float.floor() |> round()
  defp do_apply("ceiling", [value]), do: do_apply("ceil", [value])
  defp do_apply("ceil", [value]) when is_integer(value), do: value
  defp do_apply("ceil", [value]), do: value |> Float.ceil() |> round()
  defp do_apply("abs", [value]), do: abs(value)
  defp do_apply("round", [value]), do: round(value)
  defp do_apply("round", [value, _precision]) when is_integer(value), do: value
  defp do_apply("round", [value, precision]), do: Float.round(value, precision)
  defp do_apply("trunc", [value]), do: trunc(value)
  defp do_apply("trunc", [value, _precision]) when is_integer(value), do: value
  defp do_apply("trunc", [value, precision]), do: do_trunc(value, precision)
  defp do_apply("div", [x, y]), do: div(x, y)
  defp do_apply("%", [x, y]), do: rem(x, y)
  defp do_apply("mod", [x, y]), do: rem(x, y)
  defp do_apply("pow", [x, y]), do: :math.pow(x, y)
  defp do_apply("length", [string]), do: String.length(string)
  defp do_apply("lower", [string]), do: String.downcase(string)
  defp do_apply("lcase", [string]), do: String.downcase(string)
  defp do_apply("upper", [string]), do: String.upcase(string)
  defp do_apply("ucase", [string]), do: String.upcase(string)
  defp do_apply("btrim", [string]), do: trim(string, " ")
  defp do_apply("btrim", [string, chars]), do: trim(string, chars)
  defp do_apply("ltrim", [string]), do: ltrim(string, " ")
  defp do_apply("ltrim", [string, chars]), do: ltrim(string, chars)
  defp do_apply("rtrim", [string]), do: rtrim(string, " ")
  defp do_apply("rtrim", [string, chars]), do: rtrim(string, chars)
  defp do_apply("left", [string, count]), do: left(string, count)
  defp do_apply("right", [string, count]), do: right(string, count)
  defp do_apply("substring", [string, from]), do: substring(string, from)
  defp do_apply("substring", [string, from, count]), do: substring(string, from, count)
  defp do_apply("substring_for", [string, count]), do: substring(string, 1, count)
  defp do_apply("||", args), do: Enum.join(args)
  defp do_apply("concat", args), do: Enum.join(args)
  defp do_apply("^", [x, y]), do: :math.pow(x, y)
  defp do_apply("*", [x, y]), do: x * y
  defp do_apply("/", [x, y]), do: x / y
  defp do_apply("+", [x, y]), do: x + y
  defp do_apply("-", [x, y]), do: x - y
  defp do_apply({"cast", target}, [value]), do: cast(value, target)

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

  defp cast(value, :integer) when is_integer(value), do: value
  defp cast(value, :integer) when is_float(value), do: round(value)
  defp cast(true, :integer), do: 1
  defp cast(false, :integer), do: 0
  defp cast(value, :integer) when is_binary(value) do
    case Integer.parse(value) do
      {number, _rest} -> number
      :error -> nil
    end
  end
end
