defmodule Cloak.SqlQuery.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.SqlQuery.Parser
  alias Cloak.SqlQuery.Column

  import Kernel, except: [apply: 2]

  @functions %{
    ~w(count) => %{aggregate: true, type: :any},
    ~w(sum avg min max stddev median) => %{aggregate: true, type: :numeric},
    ~w(year month day hour minute second weekday) => %{aggregate: false, type: :timestamp},
    ~w(floor ceil ceiling round trunc) => %{aggregate: false, type: :real},
    ~w(abs sqrt) => %{aggregate: false, type: :numeric},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  @type t :: Parser.column | Column.t
  @type argument_type :: :any | :numeric | :timestamp


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
  @spec argument_type(t) :: argument_type
  def argument_type({:function, function, _}), do: @functions[function].type

  @doc "Returns the argument specifiaction of the given function call."
  @spec argument(t) :: Column.t
  def argument({:function, _, argument}), do: argument

  @doc "Returns the function name of the given function call."
  @spec name(Parser.column) :: String.t
  def name({:function, name, _}), do: name

  @doc "Applies the function to the database row and returns its result."
  @spec apply_to_db_row(t, DataSource.row) :: term
  def apply_to_db_row(%Column{} = column, row),
    do: Column.value(column, row)
  def apply_to_db_row(function, row) do
    true = function?(function)

    function
    |> argument()
    |> Column.value(row)
    |> apply(function)
  end

  # -------------------------------------------------------------------
  # Implementations
  # -------------------------------------------------------------------

  @doc "Returns the result of applying the given function definition to the given value."
  @spec apply(term, t) :: term
  def apply(:*, _), do: :*
  def apply(value, {:function, "year", _}), do: value.year
  def apply(value, {:function, "month", _}), do: value.month
  def apply(value, {:function, "day", _}), do: value.day
  def apply(value, {:function, "hour", _}), do: value.hour
  def apply(value, {:function, "minute", _}), do: value.minute
  def apply(value, {:function, "second", _}), do: value.second
  def apply(value, {:function, "weekday", _}), do: Timex.weekday(value)
  def apply(value, {:function, "sqrt", _}), do: :math.sqrt(value)
  def apply(value, {:function, "floor", _}), do: Float.floor(value)
  def apply(value, {:function, "ceil", _}), do: Float.ceil(value)
  def apply(value, {:function, "ceiling", _}), do: Float.ceil(value)
  def apply(value, {:function, "abs", _}), do: abs(value)
  def apply(value, {:function, "round", _}), do: round(value)
  def apply(value, {:function, "trunc", _}), do: trunc(value)
  def apply(value, _), do: value
end
