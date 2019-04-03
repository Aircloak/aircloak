defmodule Cloak.Sql.Expression do
  @moduledoc """
  Represents an expression in a compiled query. Variants of this struct can be used to represent constants, columns or
  function calls with their arguments which are expressions themselves.
  """

  alias Cloak.DataSource
  alias Cloak.Sql.{LikePattern, Query}
  alias Timex.Duration

  @type column_type :: DataSource.Table.data_type() | :like_pattern | :interval | nil
  @type function_name ::
          String.t()
          | {:cast, DataSource.Table.data_type() | :varbinary | {:native_type, String.t()}}
          | {:bucket, :lower | :upper | :middle}
  @type t :: %__MODULE__{
          table: :unknown | DataSource.Table.t(),
          name: String.t() | nil,
          alias: String.t() | nil,
          type: column_type,
          user_id?: boolean,
          row_index: nil | Query.row_index(),
          constant?: boolean,
          value: any,
          function: function_name | nil,
          function_args: [t],
          function?: boolean,
          aggregate?: boolean,
          parameter_index: pos_integer | nil,
          synthetic?: boolean,
          source_location: Cloak.Sql.Parser.location()
        }
  defstruct table: :unknown,
            name: nil,
            alias: nil,
            type: nil,
            user_id?: false,
            row_index: nil,
            constant?: false,
            value: nil,
            function: nil,
            function_args: [],
            aggregate?: false,
            function?: false,
            parameter_index: nil,
            synthetic?: false,
            source_location: nil

  @doc "Returns an expression representing a reference to the given column in the given table."
  @spec column(DataSource.column(), DataSource.table()) :: t
  def column(column, table) do
    %__MODULE__{
      table: table,
      name: column.name,
      type: column.type,
      user_id?: table.keys[column.name] == :user_id
    }
  end

  @doc """
  Returns true if the given expression represents a column reference, false otherwise. Note, that the reference might be
  to a column in a subquery, not necessarily directly to a database column.
  """
  @spec column?(t) :: boolean
  def column?(expression), do: not (expression.constant? or expression.function?)

  @doc "Returns a column struct representing the constant `value`."
  @spec constant(column_type, any, pos_integer | nil) :: t
  def constant(type, value, parameter_index \\ nil),
    do: %__MODULE__{
      constant?: true,
      value: value,
      type: normalize_type(type),
      parameter_index: parameter_index
    }

  @doc "Returns an expression representing the given like pattern with the given escape character."
  @spec like_pattern(String.t(), String.t() | nil) :: t
  def like_pattern(pattern, escape_character),
    do: constant(:like_pattern, Cloak.Sql.LikePattern.new(pattern, escape_character))

  @doc "Creates a column representing a function call."
  @spec function(function_name, [t | :* | {:distinct, t}], column_type, boolean) :: t
  def function(function_name, function_args, type, aggregate? \\ false),
    do: %__MODULE__{
      function: function_name,
      function_args: function_args,
      type: type,
      aggregate?: aggregate?,
      function?: true
    }

  @doc "Returns an expression representing a count(*)."
  @spec count_star() :: t
  def count_star(), do: function("count", [:*], :integer, true)

  @doc "Returns true if the given term is a constant column, false otherwise."
  @spec constant?(Cloak.Sql.Parser.column() | t) :: boolean
  def constant?(%__MODULE__{constant?: true}), do: true

  def constant?(%__MODULE__{function?: true, aggregate?: false, function_args: args}),
    do: Enum.all?(args, &constant?/1)

  def constant?(_), do: false

  @doc "Returns true if the expression is a function call, false otherwise."
  @spec function?(Cloak.Sql.Parser.column() | t) :: boolean
  def function?(%__MODULE__{function?: true}), do: true
  def function?(_), do: false

  @doc "Sets the source location of the given expression to the given location."
  @spec set_location(t, Cloak.Sql.Parser.location()) :: t
  def set_location(expression, location), do: %{expression | source_location: location}

  @doc "Returns the key type of the column, if any. Key types are provided by admins in the data source config file."
  @spec key_type(t) :: atom
  def key_type(%__MODULE__{user_id?: true}), do: :user_id
  def key_type(%__MODULE__{name: name, table: table}) when is_binary(name) and is_map(table), do: table.keys[name]
  def key_type(_), do: nil

  @doc "Returns true if the given column is a key, false otherwise."
  @spec key?(t) :: boolean
  def key?(expression), do: key_type(expression) != nil

  @doc """
  Returns a shorter version of the display name of the column.

  This function should mostly be used when producing error messages.
  """
  @spec short_name(t) :: String.t()
  def short_name(%__MODULE__{name: name}) when is_binary(name), do: "`#{name}`"
  def short_name(x), do: display_name(x)

  @doc """
  Returns the name of the expression.

  This function should mostly be used when producing error messages.
  """
  @spec display_name(t) :: String.t()
  def display_name(%__MODULE__{name: name, table: table}) when is_binary(name),
    do: "`#{name}` from table `#{table.name}`"

  def display_name(%__MODULE__{alias: alias}) when is_binary(alias), do: "`#{alias}`"
  def display_name(%__MODULE__{function: {:cast, _type}}), do: "`cast`"
  def display_name(%__MODULE__{function: {:bucket, _align}}), do: "`bucket`"
  def display_name(%__MODULE__{function: function}) when is_binary(function), do: "`#{function}`"

  def display_name(%__MODULE__{constant?: true, type: :interval, value: value}),
    do: "`#{Timex.Duration.to_string(value)}`"

  def display_name(%__MODULE__{constant?: true, value: value}), do: "`#{value}`"

  @doc """
  Returns the full expression as text.

  This function should mostly be used when producing error messages.
  """
  @spec display(t) :: String.t()
  def display(%__MODULE__{alias: alias} = expression) when is_binary(alias),
    do: display(%__MODULE__{expression | alias: nil}) <> " as #{alias}"

  def display(%__MODULE__{name: name}) when is_binary(name), do: name
  def display(%__MODULE__{function: {:cast, type}, function_args: [arg]}), do: "cast(#{display(arg)} as #{type})"

  def display(%__MODULE__{function: {:bucket, align}, function_args: [value, by]}),
    do: "bucket(#{display(value)} by #{display(by)} align #{align})"

  def display(%__MODULE__{function: function, function_args: [arg1, arg2]}) when function in ~w(+ - / * ^ %),
    do: "#{display(arg1)} #{function} #{display(arg2)}"

  def display(%__MODULE__{function: function, function_args: args}) when is_binary(function),
    do: "#{function}(#{args |> Enum.map(&display/1) |> Enum.join(", ")})"

  def display(%__MODULE__{constant?: true, type: :text, value: value}), do: "'#{value}'"

  def display(%__MODULE__{constant?: true, type: :interval, value: value}),
    do: "interval '#{Duration.to_string(value)}'"

  def display(%__MODULE__{constant?: true, type: type, value: value}) when type in [:date, :datetime, :time],
    do: "#{type} '#{to_string(value)}'"

  def display(%__MODULE__{constant?: true, value: value}), do: to_string(value)
  def display({:distinct, expression}), do: "distinct #{display(expression)}"
  def display(value), do: to_string(value)

  @doc "Returns the column value of a database row."
  @spec value(t, DataSource.row()) :: DataSource.field() | :*
  def value(expression, row \\ [])
  def value(%__MODULE__{constant?: true, value: value}, _row), do: value

  def value(expression = %__MODULE__{function?: true, function_args: args, row_index: nil}, row),
    do: apply_function(expression, Enum.map(args, &value(&1, row)))

  def value(%__MODULE__{row_index: nil} = column, _row),
    do: raise("Unindexed column specified: #{inspect(column, pretty: true)}")

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

    def value(%__MODULE__{row_index: unquote(position)}, unquote(matched_row)), do: unquote(matched_value)
  end

  def value(%__MODULE__{row_index: index}, row) when index >= length(row),
    do: raise("Index #{index} too large for row #{inspect(row)}")

  # Fallback to `Enum.at` for larger positions
  def value(column, row), do: Enum.at(row, column.row_index)

  @doc "Returns the value of a constant expression."
  @spec const_value(t) :: DataSource.field() | LikePattern.t()
  def const_value(%__MODULE__{constant?: true, value: value}), do: value

  def const_value(expression = %__MODULE__{function?: true, function_args: args}),
    do: apply_function(expression, Enum.map(args, &const_value/1))

  @doc "Checks two columns for equality."
  @spec equals?(any, any) :: boolean
  def equals?({:distinct, c1}, {:distinct, c2}), do: equals?(c1, c2)
  def equals?(:*, :*), do: true

  def equals?(%__MODULE__{} = c1, %__MODULE__{} = c2),
    do:
      c1.table == c2.table and c1.name == c2.name and c1.value == c2.value and c1.function == c2.function and
        Enum.zip(c1.function_args, c2.function_args)
        |> Enum.all?(fn {arg1, arg2} -> equals?(arg1, arg2) end)

  def equals?(c1, c2), do: semantic(c1) == semantic(c2)

  @doc "Returns a string id for the specified column."
  @spec id(t) :: nil | String.t()
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

  @doc "Returns the first instance of a database column from the given expression. Nil if none can be found."
  @spec first_column(t) :: t | nil
  def first_column(%__MODULE__{constant?: true}), do: nil

  def first_column(%__MODULE__{function?: true, function_args: args}) do
    args
    |> Enum.map(&first_column/1)
    |> Enum.filter(& &1)
    |> case do
      [] -> nil
      [column | _] -> column
    end
  end

  def first_column(%__MODULE__{} = column), do: column

  @doc """
  Returns the list of unique expression.
  """
  @spec unique([t]) :: [t]
  def unique(expressions), do: Enum.uniq_by(expressions, &semantic/1)

  @doc """
  Removes the alias from the given expression.

  This can be useful if we want to check whether two expressions are the same.
  """
  @spec unalias(t) :: t
  def unalias(expression), do: %__MODULE__{expression | alias: nil}

  @doc """
  Removes data from the given expression that does not contribute to its semantics for the query. Currently that's only
  source_location.
  """
  @spec semantic(t) :: t
  def semantic(expression), do: put_in(expression, [Query.Lenses.all_expressions() |> location_lens()], nil)

  @doc "Wraps a string expression in the lower case function"
  @spec lowercase(t) :: t
  def lowercase(%__MODULE__{constant?: true, type: :text, value: value} = expression),
    do: %__MODULE__{expression | value: String.downcase(value)}

  def lowercase(%__MODULE__{type: :text} = expression), do: function("lower", [expression], expression.type)

  def lowercase(%__MODULE__{type: :like_pattern, value: pattern} = expression),
    do: %__MODULE__{expression | value: LikePattern.lowercase(pattern)}

  def lowercase(_), do: raise("Only textual expressions can be made lowercase")

  @doc "Checks if a string is a valid name for a column."
  @spec valid_alias?(String.t()) :: boolean
  def valid_alias?(name),
    do:
      String.match?(name, ~r/^[_#]*[a-zA-Z][a-zA-Z0-9_.#]*$/) and not String.contains?(name, "..") and
        String.last(name) != "."

  @doc """
  Variant of `exp in [exp,...]` that discounts for differences such as source location.
  This allows us to for example see if a selected expression appears as a group by expression too.
  """
  @spec member?([t], t) :: boolean
  def member?(expressions, expression), do: Enum.any?(expressions, &equals?(&1, expression))

  @doc "Returns the title for the specified column."
  @spec title(t) :: nil | String.t()
  def title(%__MODULE__{name: name, alias: alias}), do: alias || name

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp location_lens(expression_lens) do
    Lens.match(expression_lens, fn
      expression when is_tuple(expression) -> Lens.at(3)
      expression when is_map(expression) -> Lens.keys([:source_location, :row_index])
    end)
  end

  defp apply_function(expression = %__MODULE__{function?: true}, args) do
    if Enum.member?(args, :*), do: :*, else: do_apply(expression.function, args)
  rescue
    _ -> nil
  end

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
  defp do_apply("current_datetime", []), do: NaiveDateTime.utc_now()
  defp do_apply("current_date", []), do: Date.utc_today()
  defp do_apply("current_time", []), do: Time.utc_now()
  defp do_apply("date_trunc", [spec, value]), do: date_trunc(spec, value)
  defp do_apply("sqrt", [value]), do: :math.sqrt(value)
  defp do_apply("floor", [value]) when is_integer(value), do: value
  defp do_apply("floor", [value]), do: value |> Float.floor() |> round()
  defp do_apply("ceil", [value]) when is_integer(value), do: value
  defp do_apply("ceil", [value]), do: value |> Float.ceil() |> round()
  defp do_apply("abs", [value]), do: abs(value)
  defp do_apply("round", [value]), do: round(value)
  defp do_apply("round", [value, precision]), do: do_round(value, precision)
  defp do_apply("trunc", [value]), do: trunc(value)
  defp do_apply("trunc", [value, precision]), do: do_trunc(value, precision)
  defp do_apply("div", [x, y]), do: div(x, y)
  defp do_apply("%", [x, y]), do: rem(x, y)
  defp do_apply("length", [string]), do: codepoints_length(string)
  defp do_apply("lower", [string]), do: String.downcase(string)
  defp do_apply("upper", [string]), do: String.upcase(string)
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
  defp do_apply("concat", args), do: Enum.join(args)
  defp do_apply("hex", [string]), do: Base.encode16(string, case: :lower)
  defp do_apply("dec_b64", [nil]), do: nil

  defp do_apply("dec_b64", [string]) do
    case Base.decode64(string, ignore: :whitespace, padding: false) do
      {:ok, string} -> string
      :error -> nil
    end
  end

  defp do_apply("dec_aes_cbc128", [string]), do: dec_aes_cbc128(string, Application.get_env(:cloak, :aes_key))

  defp do_apply("dec_aes_cbc128", [string, key]), do: dec_aes_cbc128(string, key)

  defp do_apply("hash", [value]) do
    <<_::16, hash::binary-4, _::80>> = :crypto.hash(:md5, to_string(value))
    Base.encode16(hash, case: :lower)
  end

  defp do_apply("bool_op", [_op, nil, _any]), do: nil
  defp do_apply("bool_op", [_op, _any, nil]), do: nil
  defp do_apply("bool_op", ["=", arg1, arg2]), do: arg1 == arg2
  defp do_apply("bool_op", ["<>", arg1, arg2]), do: arg1 != arg2
  defp do_apply("bool_op", [">", arg1, arg2]), do: arg1 > arg2
  defp do_apply("bool_op", [">=", arg1, arg2]), do: arg1 >= arg2
  defp do_apply("bool_op", ["<", arg1, arg2]), do: arg1 < arg2
  defp do_apply("bool_op", ["<=", arg1, arg2]), do: arg1 <= arg2
  defp do_apply("bool_op", [op, _arg1, _arg2]), do: raise("Invalid boolean operator `#{op}` specified!")

  defp do_apply("extract_words", [nil]), do: [nil]
  defp do_apply("extract_words", [string]), do: String.split(string)
  defp do_apply("^", [x, y]), do: :math.pow(x, y)
  defp do_apply("*", [x = %Duration{}, y]), do: Duration.scale(x, y)
  defp do_apply("*", [x, y = %Duration{}]), do: do_apply("*", [y, x])
  defp do_apply("*", [x, y]), do: x * y
  defp do_apply("/", [x = %Duration{}, y]), do: Duration.scale(x, 1 / y)
  defp do_apply("/", [x, y]), do: x / y

  defp do_apply("+", [x = %Date{}, y = %Duration{}]), do: x |> Timex.to_naive_datetime() |> Timex.add(y)

  defp do_apply("+", [x = %NaiveDateTime{}, y = %Duration{}]), do: Timex.add(x, y)
  defp do_apply("+", [x = %Duration{}, y = %Duration{}]), do: Duration.add(x, y)
  defp do_apply("+", [x = %Duration{}, y]), do: do_apply("+", [y, x])
  defp do_apply("+", [x = %Time{}, y = %Duration{}]), do: add_to_time(x, y)
  defp do_apply("+", [x, y]), do: x + y
  defp do_apply("-", [x = %Date{}, y = %Date{}]), do: Timex.diff(x, y, :duration)

  defp do_apply("-", [x = %NaiveDateTime{}, y = %NaiveDateTime{}]), do: Timex.diff(x, y, :duration)

  defp do_apply("-", [x = %Time{}, y = %Time{}]), do: Duration.sub(Duration.from_time(x), Duration.from_time(y))

  defp do_apply("-", [x, y = %Duration{}]), do: do_apply("+", [x, Duration.scale(y, -1)])
  defp do_apply("-", [x, y]), do: x - y
  defp do_apply({:cast, target}, [value]), do: cast(value, target)
  defp do_apply({:bucket, :lower}, [value, bucket_size]), do: Float.floor(value / bucket_size) * bucket_size

  defp do_apply({:bucket, :upper}, [value, bucket_size]),
    do: do_apply({:bucket, :lower}, [value, bucket_size]) + bucket_size

  defp do_apply({:bucket, :middle}, [value, bucket_size]),
    do: Float.floor(value / bucket_size) * bucket_size + 0.5 * bucket_size

  defp do_apply("coalesce", values), do: Enum.find(values, & &1)

  defp do_trunc(value, 0), do: trunc(value)

  defp do_trunc(value, precision) when precision < 0,
    do: trunc(value * :math.pow(10, precision)) * :math.pow(10, -precision)

  defp do_trunc(value, _precision) when is_integer(value), do: value

  defp do_trunc(value, precision) when value < 0, do: value |> :erlang.float() |> Float.ceil(precision)

  defp do_trunc(value, precision), do: value |> :erlang.float() |> Float.floor(precision)

  defp do_round(value, precision) when precision < 0,
    do: round(value * :math.pow(10, precision)) * :math.pow(10, -precision)

  defp do_round(value, _precision) when is_integer(value), do: value
  defp do_round(value, precision), do: Float.round(value, precision)

  defp left(nil, _), do: nil
  defp left(_, nil), do: nil
  defp left(string, count) when count < 0, do: slice_codepoints(string, 0, max(codepoints_length(string) + count, 0))
  defp left(string, count), do: slice_codepoints(string, 0, count)

  defp right(nil, _), do: nil
  defp right(_, nil), do: nil
  defp right(string, count) when count < 0, do: slice_codepoints(string, -count, codepoints_length(string))
  defp right(string, count), do: slice_codepoints(string, (codepoints_length(string) - count) |> max(0), count)

  defp trim(string, ""), do: string
  defp trim(string, chars), do: string |> ltrim(chars) |> rtrim(chars)

  defp ltrim(string, ""), do: string
  defp ltrim(string, chars), do: Regex.replace(~r/^[#{Regex.escape(chars)}]*/, string, "")

  defp rtrim(string, ""), do: string
  defp rtrim(string, chars), do: Regex.replace(~r/[#{Regex.escape(chars)}]*$/, string, "")

  defp codepoints_length(string), do: string |> String.codepoints() |> length()

  @max_precision_zero {0, 6}
  @midnight ~T[00:00:00.000000]
  defp date_trunc(scope, %Time{}) when scope in ~w(year quarter month day), do: @midnight
  defp date_trunc("year", date), do: date_trunc("month", %{date | month: 1})
  defp date_trunc("quarter", date), do: date_trunc("month", %{date | month: first_month_of_quarter(date)})
  defp date_trunc("month", date), do: date_trunc("day", %{date | day: 1})
  defp date_trunc(_, date = %Date{}), do: date
  defp date_trunc("day", date), do: date_trunc("hour", %{date | hour: 0})
  defp date_trunc("hour", date), do: date_trunc("minute", %{date | minute: 0})
  defp date_trunc("minute", date), do: date_trunc("second", %{date | second: 0})
  defp date_trunc("second", date), do: %{date | microsecond: @max_precision_zero}

  @month_in_quarter 3
  defp first_month_of_quarter(%{month: month}), do: div(month - 1, @month_in_quarter) * @month_in_quarter + 1

  defp substring(string, from, count \\ nil)
  defp substring(nil, _, _), do: nil
  defp substring(string, from, nil), do: substring(string, from, codepoints_length(string))
  defp substring(string, from, count), do: slice_codepoints(string, from - 1, count)

  defp slice_codepoints(string, from, count) do
    string
    |> String.codepoints()
    |> Stream.drop(from)
    |> Enum.take(count)
    |> to_string()
  end

  defp add_to_time(time, duration) do
    NaiveDateTime.from_erl!({_arbitrary_date = {100, 1, 1}, Time.to_erl(time)})
    |> Timex.add(duration_time_part(duration))
    |> NaiveDateTime.to_time()
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
    :erlang.float(value)
  rescue
    _ in ArgumentError -> nil
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

  defp cast(value = %Date{}, :datetime), do: value |> NaiveDateTime.new(~T[00:00:00.000000]) |> error_to_nil()

  defp cast(value, :datetime) when is_binary(value), do: value |> Cloak.Time.parse_datetime() |> error_to_nil()

  # cast to time
  defp cast(value = %Time{}, :time), do: value
  defp cast(value = %NaiveDateTime{}, :time), do: NaiveDateTime.to_time(value)

  defp cast(value, :time) when is_binary(value), do: value |> Cloak.Time.parse_time() |> error_to_nil()

  # cast to date
  defp cast(value = %Date{}, :date), do: value
  defp cast(value = %NaiveDateTime{}, :date), do: NaiveDateTime.to_date(value)

  defp cast(value, :date) when is_binary(value), do: value |> Cloak.Time.parse_date() |> error_to_nil()

  # cast to interval
  defp cast(value = %Duration{}, :interval), do: value

  defp cast(value, :interval) when is_binary(value), do: value |> Duration.parse() |> error_to_nil()

  defp duration_time_part(duration) do
    {hours, days, seconds, microseconds} = Duration.to_clock(duration)
    Duration.from_clock({rem(hours, 24), days, seconds, microseconds})
  end

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _}), do: nil

  @zero_iv String.duplicate(<<0>>, 16)
  defp dec_aes_cbc128(value, key) when value in [nil, ""] or key in [nil, ""], do: nil
  defp dec_aes_cbc128(value, _key) when rem(byte_size(value), 16) != 0, do: nil

  defp dec_aes_cbc128(value, key) do
    value = :crypto.block_decrypt(:aes_cbc128, key, @zero_iv, value)
    last = :binary.last(value)
    {value, padding} = String.split_at(value, -last)
    if padding == String.duplicate(<<last>>, last), do: value, else: nil
  end
end
