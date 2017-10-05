defmodule Cloak.Sql.Expression do
  @moduledoc """
  Represents an expression in a compiled query. Variants of this struct can be used to represent constants, columns or
  function calls with their arguments which are expressions themselves.
  """

  alias Cloak.DataSource
  alias Cloak.Sql.LikePattern
  alias Timex.Duration

  @type column_type :: DataSource.Table.data_type | :like_pattern | :interval | nil
  @type function_name ::
    String.t |
    {:cast, DataSource.Table.data_type | :varbinary} |
    {:bucket, :lower | :upper | :middle}
  @type t :: %__MODULE__{
    table: :unknown | DataSource.Table.t,
    name: String.t | nil,
    alias: String.t | nil,
    type: column_type,
    user_id?: boolean,
    row_index: nil | Cloak.Sql.Query.row_index,
    constant?: boolean,
    value: any,
    function: function_name | nil,
    function_args: [t],
    function?: boolean,
    aggregate?: boolean,
    parameter_index: pos_integer | nil,
    visible?: boolean,
    key?: boolean,
  }
  defstruct [
    table: :unknown, name: nil, alias: nil, type: nil, user_id?: false, row_index: nil, constant?: false,
    value: nil, function: nil, function_args: [], aggregate?: false, function?: false, parameter_index: nil,
    visible?: true, key?: false
  ]

  @doc "Returns an expression representing a reference to the given column in the given table."
  @spec column(DataSource.column, DataSource.table) :: t
  def column(column, table), do:
    %__MODULE__{
      table: table,
      name: column.name,
      type: column.type,
      user_id?: table.user_id == column.name,
      key?: column.name in Map.get(table, :keys, []),
    }

  @doc "Returns a column struct representing the constant `value`."
  @spec constant(column_type, any, pos_integer | nil) :: t
  def constant(type, value, parameter_index \\ nil), do:
    %__MODULE__{
      constant?: true, value: value, type: normalize_type(type), parameter_index: parameter_index
    }

  @doc "Returns an expression representing the given like pattern with the given escape character."
  @spec like_pattern(String.t, String.t | nil) :: t
  def like_pattern(pattern, escape_character), do:
    constant(:like_pattern, {pattern, escape_character})

  @doc "Creates a column representing a function call."
  @spec function(function_name, [t | :*], column_type, boolean) :: t
  def function(function_name, function_args, type \\ nil, aggregate? \\ false), do:
    %__MODULE__{
      function: function_name, function_args: function_args, type: type, aggregate?: aggregate?, function?: true
    }

  @doc "Returns an expression representing a count(*)."
  @spec count_star() :: t
  def count_star(), do: function("count", [:*], nil, true)

  @doc "Returns true if the given term is a constant column, false otherwise."
  @spec constant?(Cloak.Sql.Parser.column | t) :: boolean
  def constant?(%__MODULE__{constant?: true}), do: true
  def constant?(%__MODULE__{function?: true, function_args: args}), do: Enum.all?(args, &constant?/1)
  def constant?(_), do: false

  @doc "Returns true if the given column is a key (public/private/user_id), false otherwise."
  @spec key?(t) :: boolean
  def key?(%__MODULE__{user_id?: true}), do: true
  def key?(%__MODULE__{key?: key}), do: key

  @doc """
  Returns a shorter version of the display name of the column.

  This function should mostly be used when producing error messages.
  """
  @spec short_name(t) :: String.t
  def short_name(%__MODULE__{name: name}) when is_binary(name), do: "`#{name}`"
  def short_name(x), do: display_name(x)

  @doc """
  Returns a display name of the column.

  This function should mostly be used when producing error messages.
  """
  @spec display_name(t) :: String.t
  def display_name(%__MODULE__{name: name, table: table}) when is_binary(name), do:
    "`#{name}` from table `#{table.name}`"
  def display_name(%__MODULE__{alias: alias}) when is_binary(alias), do: "`#{alias}`"
  def display_name(%__MODULE__{function: {:cast, _type}}), do: "`cast`"
  def display_name(%__MODULE__{function: function}) when is_binary(function), do: "`#{function}`"
  def display_name(%__MODULE__{constant?: true, value: value}), do: "`#{value}`"

  @doc "Returns the column value of a database row."
  @spec value(t, DataSource.row) :: DataSource.field | LikePattern.t
  def value(expression, row \\ [])
  def value(%__MODULE__{constant?: true, value: value}, _row), do: value
  def value(expression = %__MODULE__{function?: true, function_args: args, row_index: nil}, row), do:
    apply_function(expression, Enum.map(args, &value(&1, row)))
  def value(%__MODULE__{row_index: nil} = column, _row), do:
    raise "Unindexed column specified: #{inspect(column, pretty: true)}"
  for position <- 0..99 do
    # Generates pattern matching clauses to improve sequential access to a value:
    #
    #   defp value(%__MODULE__{row_index: 0}, [el | _]), do: el
    #   defp value(%__MODULE__{row_index: 1}, [_, el | _]), do: el
    #   defp value(%__MODULE__{row_index: 2}, [_, _, el | _]), do: el
    #   ...
    #
    # This works faster than `Enum.at`, especially if positions are larger.
    matched_value = quote do: value
    matched_row_head = List.duplicate(quote(do: _), position) ++ [matched_value]
    matched_row = quote do: [unquote_splicing(matched_row_head) | _]

    def value(%__MODULE__{row_index: unquote(position)}, unquote(matched_row)),
      do: unquote(matched_value)
  end
  def value(%__MODULE__{row_index: index}, row) when index >= length(row),
    do: raise "Index #{index} too large for row #{inspect(row)}"
  # Fallback to `Enum.at` for larger positions
  def value(column, row), do: Enum.at(row, column.row_index)

  @doc "Returns the value of a constant expression."
  @spec const_value(t) :: DataSource.field | LikePattern.t
  def const_value(%__MODULE__{constant?: true, value: value}), do: value
  def const_value(expression = %__MODULE__{function?: true, function_args: args}), do:
    apply_function(expression, Enum.map(args, &const_value/1))

  @doc "Checks two columns for equality."
  @spec equals(any, any) :: boolean
  def equals({:distinct, c1}, {:distinct, c2}), do: equals(c1, c2)
  def equals(:*, :*), do: true
  def equals(%__MODULE__{} = c1, %__MODULE__{} = c2), do:
    c1.table == c2.table and
    c1.name == c2.name and
    c1.value == c2.value and
    c1.function == c2.function and
    Enum.zip(c1.function_args, c2.function_args) |> Enum.all?(fn ({arg1, arg2}) -> equals(arg1, arg2) end)
  def equals(_c1, _c2), do: false

  @doc "Returns a string id for the specified column."
  @spec id(t) :: nil | String.t
  def id(%__MODULE__{table: :unknown, name: nil, alias: alias}), do: alias
  def id(%__MODULE__{table: :unknown, name: name}), do: name
  def id(%__MODULE__{table: table, name: name}), do: "#{table.name}.#{name}"

  @doc "Returns the list of arguments if the given Expression is a function expression, [] otherwise."
  @spec arguments(t) :: [t]
  def arguments(%__MODULE__{function?: true, function_args: args}), do: args
  def arguments(_), do: []

  @doc "Returns the first argument of the function expression."
  @spec first_argument!(t) :: t
  def first_argument!(%__MODULE__{function?: true, function_args: [arg | _]}), do: arg

  @doc "Returns the result of applying the given function expression to the given list of arguments."
  @spec apply_function(t, [any]) :: any
  def apply_function(expression = %__MODULE__{function?: true}, args) do
    try do
      if Enum.member?(args, :*), do: :*, else: do_apply(expression.function, args)
    rescue
      _ -> nil
    end
  end

  @doc "Returns the first instance of a database column from the given expression. Nil if none can be found."
  @spec first_column(t) :: t | nil
  def first_column(%__MODULE__{constant?: true}), do: nil
  def first_column(%__MODULE__{function?: true, function_args: args}) do
    args
    |> Enum.map(&first_column/1)
    |> Enum.filter(&(&1))
    |> case do
      [] -> nil
      [column|_] -> column
    end
  end
  def first_column(%__MODULE__{} = column), do: column

  @doc "Returns true if the given expression is the row splitting function."
  @spec row_splitter?(t) :: boolean
  def row_splitter?(%__MODULE__{function?: true} = function), do:
    Cloak.Sql.Function.has_attribute?(function, :row_splitter)
  def row_splitter?(_), do: false

  @doc """
  Returns a list of all splitters used in the given expressions.

  The splitters are returned in post-order, meaning that a nested splitter will always precede its ancestors.
  """
  @spec all_splitters(t) :: [t]
  def all_splitters(%__MODULE__{function?: false}), do:
    []
  def all_splitters(function) do
    this_splitter = if row_splitter?(function), do: [function], else: []
    nested_splitters =
      function
      |> arguments()
      |> Enum.flat_map(&all_splitters/1)

    Enum.concat([nested_splitters, this_splitter])
  end

  @doc """
  Returns the list of unique expression, preserving duplicates of some expressions.

  Expressions for which the provided lambda returns true are not deduplicated.
  """
  @spec unique_except([t], ((t) -> boolean)) :: [t]
  def unique_except(expressions, except_fun), do:
    Enum.uniq_by(
      expressions,
      fn(expression) -> if except_fun.(expression), do: :erlang.unique_integer(), else: expression end
    )

  @doc """
  Removes the alias from the given expression.

  This can be useful if we want to check whether two expressions are the same.
  """
  @spec unalias(t) :: t
  def unalias(expression), do:
    %__MODULE__{expression | alias: nil}


  @doc "Recursively evaluates a split expression and returns all the values yielded by the splitter."
  @spec splitter_values(t, DataSource.row) :: [DataSource.field]
  def splitter_values(splitter, row), do:
    Enum.map(
      eval_split_expression(splitter, row),
      fn(%__MODULE__{constant?: true, value: value}) -> value end
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp normalize_type(:string), do: :text
  defp normalize_type(:float), do: :real
  defp normalize_type(type), do: type

  defp do_apply("year", [value]), do: value.year
  defp do_apply("quarter", [value]), do: div(value.month - 1, 3) + 1
  defp do_apply("month", [value]), do: value.month
  defp do_apply("day", [value]), do: value.day
  defp do_apply("hour", [value]), do: value.hour
  defp do_apply("minute", [value]), do: value.minute
  defp do_apply("second", [value]), do: value.second
  defp do_apply("weekday", [value]), do: Timex.weekday(value)
  defp do_apply("date_trunc", [spec, value]), do: date_trunc(spec, value)
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
  defp do_apply("concat", args), do: Enum.join(args)
  defp do_apply("hex", [string]), do: Base.encode16(string, case: :lower)
  defp do_apply("hash", [value]) do
    <<hash::60, _::4, _::64>> = :crypto.hash(:md5, to_string(value))
    hash
  end
  defp do_apply("extract_match", [string, regex]) do
    case Regex.run(regex, string, capture: :first) do
      [capture] -> capture
      nil -> nil
    end
  end
  defp do_apply("extract_matches", [nil, _regex]), do: [nil]
  defp do_apply("extract_matches", [string, regex]), do:
    List.flatten(Regex.scan(regex, string, capture: :first))
  defp do_apply("^", [x, y]), do: :math.pow(x, y)
  defp do_apply("*", [x = %Duration{}, y]), do: Duration.scale(x, y)
  defp do_apply("*", [x, y = %Duration{}]), do: do_apply("*", [y, x])
  defp do_apply("*", [x, y]), do: x * y
  defp do_apply("/", [x = %Duration{}, y]), do: Duration.scale(x, 1 / y)
  defp do_apply("/", [x, y]), do: x / y
  defp do_apply("+", [x = %Date{}, y = %Duration{}]), do:
    x |> Timex.to_naive_datetime() |> Timex.add(y)
  defp do_apply("+", [x = %NaiveDateTime{}, y = %Duration{}]), do:
    Timex.add(x, y)
  defp do_apply("+", [x = %Duration{}, y = %Duration{}]), do: Duration.add(x, y)
  defp do_apply("+", [x = %Duration{}, y]), do: do_apply("+", [y, x])
  defp do_apply("+", [x = %Time{}, y = %Duration{}]), do: add_to_time(x, y)
  defp do_apply("+", [x, y]), do: x + y
  defp do_apply("-", [x = %Date{}, y = %Date{}]), do: Timex.diff(x, y, :duration)
  defp do_apply("-", [x = %NaiveDateTime{}, y = %NaiveDateTime{}]), do: Timex.diff(x, y, :duration)
  defp do_apply("-", [x = %Time{}, y = %Time{}]), do:
    Duration.sub(Duration.from_time(x), Duration.from_time(y))
  defp do_apply("-", [x, y = %Duration{}]), do: do_apply("+", [x, Duration.scale(y, -1)])
  defp do_apply("-", [x, y]), do: x - y
  defp do_apply({:cast, target}, [value]), do: cast(value, target)
  defp do_apply("coalesce", values), do: Enum.find(values, &(&1))

  defp do_trunc(value, 0), do: trunc(value)
  defp do_trunc(value, precision) when value < 0, do: value |> :erlang.float() |> Float.ceil(precision)
  defp do_trunc(value, precision), do: value |> :erlang.float() |> Float.floor(precision)

  defp left(nil, _), do: nil
  defp left(_, nil), do: nil
  defp left(string, count) when count < 0, do:
    String.slice(string, 0, max(String.length(string) + count, 0))
  defp left(string, count), do: String.slice(string, 0, count)

  defp right(nil, _), do: nil
  defp right(_, nil), do: nil
  defp right(string, count) when count < 0, do: String.slice(string, -count, String.length(string))
  defp right(string, count), do: String.slice(string, String.length(string) - count |> max(0), count)

  defp trim(string, chars), do: string |> ltrim(chars) |> rtrim(chars)

  defp ltrim(string, chars), do: Regex.replace(~r/^[#{Regex.escape(chars)}]*/, string, "")

  defp rtrim(string, chars), do: Regex.replace(~r/[#{Regex.escape(chars)}]*$/, string, "")

  @max_precision_zero {0, 6}
  @midnight ~T[00:00:00.000000]
  defp date_trunc(scope, %Time{}) when scope in ~w(year quarter month day), do: @midnight
  defp date_trunc("year", date), do: date_trunc("month", %{date | month: 1})
  defp date_trunc("quarter", date), do: date_trunc("month", %{date | month: first_month_of_quarter(date)})
  defp date_trunc("month", date), do: date_trunc("day", %{date | day: 1})
  defp date_trunc("day", date), do: date_trunc("hour", %{date | hour: 0})
  defp date_trunc("hour", date), do: date_trunc("minute", %{date | minute: 0})
  defp date_trunc("minute", date), do: date_trunc("second", %{date | second: 0})
  defp date_trunc("second", date), do: %{date | microsecond: @max_precision_zero}

  @month_in_quarter 3
  defp first_month_of_quarter(%{month: month}), do: div(month - 1, @month_in_quarter) * @month_in_quarter + 1

  defp substring(string, from, count \\ nil)
  defp substring(nil, _, _), do: nil
  defp substring(_, nil, _), do: nil
  defp substring(string, from, count) when from < 1, do: substring(string, 1, count + from - 1)
  defp substring(_string, _from, count) when count < 0, do: ""
  defp substring(string, from, count), do:
    String.slice(string, from - 1, count || String.length(string))

  defp add_to_time(time, duration) do
    NaiveDateTime.from_erl!({_arbitrary_date = {100, 1, 1}, Time.to_erl(time)})
    |> Timex.add(duration_time_part(duration))
    |> NaiveDateTime.to_time
  end

  defp cast(nil, _), do: nil
  # cast to integer
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
  # cast to real
  defp cast(value, :real) when is_integer(value) do
    try do
      :erlang.float(value)
    rescue
      _ in ArgumentError -> nil
    end
  end
  defp cast(value, :real) when is_float(value), do: value
  defp cast(true, :real), do: 1.0
  defp cast(false, :real), do: 0.0
  defp cast(value, :real) when is_binary(value) do
    case Float.parse(value) do
      {number, _rest} -> number
      :error -> nil
    end
  end
  # cast to text
  defp cast(true, :text), do: "TRUE"
  defp cast(false, :text), do: "FALSE"
  defp cast(value = %Duration{}, :text), do: Duration.to_string(value)
  defp cast(value = %NaiveDateTime{}, :text) do
    case Timex.format(value, "{ISOdate} {ISOtime}") do
      {:ok, result} -> result
      {:error, _} -> nil
    end
  end
  defp cast(value, :text), do: to_string(value)
  # cast to boolean
  defp cast(value, :boolean) when is_integer(value), do: value != 0
  defp cast(value, :boolean) when is_float(value), do: round(value) != 0
  defp cast(value, :boolean) when is_boolean(value), do: value
  defp cast(value, :boolean) when is_binary(value) do
    case String.downcase(value) do
      "true" -> true
      "false" -> false
      _ -> nil
    end
  end
  # cast to datetime
  defp cast(value = %NaiveDateTime{}, :datetime), do: value
  defp cast(value, :datetime) when is_binary(value), do:
    value |> Cloak.Time.parse_datetime() |> error_to_nil()
  # cast to time
  defp cast(value = %Time{}, :time), do: value
  defp cast(value = %NaiveDateTime{}, :time), do: NaiveDateTime.to_time(value)
  defp cast(value, :time) when is_binary(value), do:
    value |> Cloak.Time.parse_time() |> error_to_nil()
  # cast to date
  defp cast(value = %Date{}, :date), do: value
  defp cast(value = %NaiveDateTime{}, :date), do: NaiveDateTime.to_date(value)
  defp cast(value, :date) when is_binary(value), do:
    value |> Cloak.Time.parse_date() |> error_to_nil()
  # cast to interval
  defp cast(value = %Duration{}, :interval), do: value
  defp cast(value, :interval) when is_binary(value), do:
    value |> Duration.parse() |> error_to_nil()

  defp duration_time_part(duration) do
    {hours, days, seconds, microseconds} = Duration.to_clock(duration)
    Duration.from_clock({rem(hours, 24), days, seconds, microseconds})
  end

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _}), do: nil

  defp eval_split_expression(%__MODULE__{constant?: true} = expression, _row), do:
    [expression]
  defp eval_split_expression(%__MODULE__{function?: false} = expression, row), do:
    [constant(expression.type, value(expression, row))]
  defp eval_split_expression(%__MODULE__{function?: true} = expression, row) do
    apply_args_instances(expression, eval_args(expression.function_args, row))
  end

  defp eval_args([], _row), do: [[]]
  defp eval_args([arg | rest], row) do
    for arg_value <- eval_split_expression(arg, row), remaining_arg_values <- eval_args(rest, row), do:
      [arg_value | remaining_arg_values]
  end

  defp apply_args_instances(function, args_instances) do
    Enum.flat_map(args_instances, &invoke_fun(function, &1))
  end

  defp invoke_fun(function, args), do:
    function
    |> function_results(args)
    |> Enum.map(&constant(function.type, &1))

  defp function_results(function, args) do
    if row_splitter?(function) do
      value(%{function | function_args: args, row_index: nil})
    else
      [value(%{function | function_args: args})]
    end
  end
end
