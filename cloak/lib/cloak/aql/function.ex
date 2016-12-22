defmodule Cloak.Aql.Function do
  @moduledoc "Includes information about functions and implementation of non-aggregation functions."

  alias Cloak.Aql.{Expression, Parser}
  alias Cloak.DataSource
  alias Timex.Duration

  import Kernel, except: [apply: 2]

  numeric = {:or, [:integer, :real]}
  arithmetic_operation = %{
    [:integer, :integer] => :integer,
    [:real, :integer] => :real,
    [:integer, :real] => :real,
    [:real, :real] => :real,
  }

  @functions %{
    ~w(count count_noise) => %{aggregate: true, type_specs: %{[:any] => :integer}},
    ~w(sum sum_noise) => %{aggregate: true, type_specs: %{
      [:integer] => :integer,
      [:real] => :real,
    }},
    ~w(min max median) => %{aggregate: true, type_specs: %{
      [:integer] => :integer,
      [:real] => :real,
      [:date] => :date,
      [:time] => :time,
      [:datetime] => :datetime,
      [:text] => :text,
    }},
    ~w(avg stddev avg_noise stddev_noise) => %{aggregate: true, type_specs: %{[numeric] => :real}},
    ~w(hour minute second) =>
      %{type_specs: %{[{:or, [:datetime, :time]}] => :integer}},
    ~w(year month day weekday) =>
      %{type_specs: %{[{:or, [:datetime, :date]}] => :integer}},
    ~w(floor ceil ceiling) => %{type_specs: %{[numeric] => :integer}},
    ~w(round trunc) => %{type_specs: %{
      [numeric] => :integer,
      [numeric, :integer] => :real,
    }},
    [{:bucket, :lower}, {:bucket, :upper}, {:bucket, :middle}] => %{type_specs: %{
      [numeric, numeric] => :real,
    }},
    ~w(abs) => %{type_specs: %{[numeric] => :real}},
    ~w(sqrt) => %{type_specs: %{[numeric] => :real}},
    ~w(div mod %) => %{type_specs: %{[:integer, :integer] => :integer}},
    ~w(pow ^) => %{type_specs: %{[numeric, numeric] => :real}},
    ~w(+) => %{type_specs: Map.merge(arithmetic_operation, %{
      [:date, :interval] => :datetime,
      [:time, :interval] => :time,
      [:datetime, :interval] => :datetime,
      [:interval, :date] => :datetime,
      [:interval, :time] => :time,
      [:interval, :datetime] => :datetime,
      [:interval, :interval] => :interval,
    })},
    ~w(-) => %{type_specs: Map.merge(arithmetic_operation, %{
      [:date, :date] => :interval,
      [:time, :time] => :interval,
      [:datetime, :datetime] => :interval,
      [:date, :interval] => :datetime,
      [:time, :interval] => :time,
      [:datetime, :interval] => :datetime,
      [:interval, :interval] => :interval,
    })},
    ~w(*) => %{type_specs: Map.merge(arithmetic_operation, %{
       [:interval, numeric] => :interval,
       [numeric, :interval] => :interval,
     })},
    ~w(/) => %{type_specs: %{
      [numeric, numeric] => :real,
      [:interval, {:or, [:integer, :real]}] => :interval,
    }},
    ~w(length) => %{type_specs: %{[:text] => :integer}},
    ~w(lower lcase upper ucase) => %{type_specs: %{[:text] => :text}},
    ~w(left right) => %{type_specs: %{[:text, :integer] => :text}},
    ~w(btrim ltrim rtrim) => %{type_specs: %{[:text, {:optional, :text}] => :text}},
    ~w(substring substring_for) =>
      %{type_specs: %{[:text, :integer, {:optional, :integer}] => :text}},
    ~w(||) => %{type_specs: %{[:text, :text] => :text}},
    ~w(concat) => %{type_specs: %{[{:many1, :text}] => :text}},
    # NOTICE: The `not_in_subquery` needs to be set for `extract_match` and `extract_matches`
    # (whether or not we can implement it in a subquery). The reason for this is what we have called:
    # WYSIWYC (what you see is what you count). If `extract_match` is allowed in a subquery it can
    # effectively be used as an `OR`, whereas if it is used at the top-level the output of
    # the function is directly anonymised, and hence safe.
    # An example attack could look like:
    #
    #   SELECT count(*)
    #   FROM (
    #     SELECT uid, extract_match(name, 'Pattern1|Pattern2|Pattern3|...') as match
    #     FROM table
    #     WHERE match is not null
    #   )
    #
    ~w(extract_match) => %{type_specs: %{[:text, :text] => :text}, not_in_subquery: true,
      precompiled: true},
    ~w(extract_matches) => %{type_specs: %{[:text, :text] => :text}, not_in_subquery: true,
      precompiled: true, row_splitting: true},
    [{:cast, :integer}] =>
      %{type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :integer}},
    [{:cast, :real}] =>
      %{type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :real}},
    [{:cast, :boolean}] =>
      %{type_specs: %{[{:or, [:real, :integer, :text, :boolean]}] => :boolean}},
    [{:cast, :datetime}] =>
      %{type_specs: %{[{:or, [:text, :datetime]}] => :datetime}},
    [{:cast, :time}] =>
      %{type_specs: %{[{:or, [:text, :datetime, :time]}] => :time}},
    [{:cast, :date}] =>
      %{type_specs: %{[{:or, [:text, :datetime, :date]}] => :date}},
    [{:cast, :text}] =>
      %{type_specs: %{[:any] => :text}},
    [{:cast, :interval}] =>
      %{type_specs: %{[{:or, [:text, :interval]}] => :interval}},
  }
  |> Enum.flat_map(fn({functions, traits}) -> Enum.map(functions, &{&1, traits}) end)
  |> Enum.into(%{})

  @type t :: Parser.column | Expression.t
  @type data_type :: :any | DataSource.data_type
  @type argument_type :: data_type | {:optional, data_type} | {:many1, data_type} | {:or, [data_type]}
  @type function_compilation_callback :: (list(t)) :: list(t) | no_return


  # -------------------------------------------------------------------
  # Info functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column definition is a function call, false otherwise."
  @spec function?(t) :: boolean
  def function?({:function, _, _}), do: true
  def function?(_), do: false

  @doc """
  Returns true if the given column definition is a function call to an aggregate function, false otherwise.
  """
  @spec aggregate_function?(t) :: boolean
  def aggregate_function?({:function, name, _}), do: @functions |> Map.fetch!(name) |> Map.get(:aggregate, false)
  def aggregate_function?(_), do: false

  @doc "Returns true if the function is one that can split a row into multiple rows, false otherise."
  @spec row_splitting_function?(t) :: boolean
  def row_splitting_function?({:function, name, _}), do: @functions |> Map.fetch!(name) |> Map.get(:row_splitting, false)
  def row_splitting_function?(_), do: false

  @doc "Returns true if the given function call is a cast, false otherwise."
  @spec cast?(t) :: boolean
  def cast?({:function, {:cast, _}, _}), do: true
  def cast?(_), do: false

  @doc "Returns a list of possible argument lists required by the given function call."
  @spec argument_types(t) :: [[argument_type]]
  def argument_types({:function, function, _}), do: @functions[function].type_specs |> Map.keys()

  @doc "Returns the argument specifiaction of the given function call."
  @spec arguments(t) :: [Expression.t]
  def arguments({:function, _, arguments}), do: arguments
  def arguments(_), do: []

  @doc "Returns the function name of the given function call."
  @spec name(t) :: String.t
  def name(%Expression{function: {:cast, _}}), do: "cast"
  def name(%Expression{function: {:bucket, _}}), do: "bucket"
  def name(%Expression{function: name}), do: name

  @doc "Returns the return type of the given function call."
  @spec return_type(t) :: data_type | nil
  def return_type({:function, {:cast, type}, _}), do: type
  def return_type(function = {:function, name, _}) do
    @functions[name].type_specs
    |> Enum.find(fn({arguments, _}) -> do_well_typed?(function, arguments) end)
    |> case do
      {_arguments, return_type} -> return_type
      nil -> nil
    end
  end

  @doc "Returns the type of the given expression."
  @spec type(t) :: data_type
  def type(function = {:function, _, _}), do: return_type(function)
  def type({column, :as, _}), do: type(column)
  def type({:distinct, column}), do: type(column)
  def type(%Expression{type: type}), do: type
  def type(:*), do: :any

  @doc "Returns true if the arguments to the given function call match the expected argument types, false otherwise."
  @spec well_typed?(t) :: boolean
  def well_typed?(column), do:
    if function?(column),
      do: Enum.any?(argument_types(column), &do_well_typed?(column, &1)),
      else: true

  @doc "Applies the function to the database row and returns its result."
  @spec apply_to_db_row(t, DataSource.row) :: term
  def apply_to_db_row(%Expression{} = column, row),
    do: Expression.value(column, row)
  def apply_to_db_row({:distinct, column}, row),
    do: apply_to_db_row(column, row)
  def apply_to_db_row(function, row) do
    true = function?(function)

    function
    |> arguments()
    |> Enum.map(&apply_to_db_row(&1, row))
    |> apply(function)
  end

  @doc "Returns the result of applying the given function definition to the given arguments."
  @spec apply([term], t) :: term
  def apply(args, {:function, name, _}) do
    try do
      if Enum.member?(args, :*), do: :*, else: do_apply(name, args)
    rescue
      _ -> nil
    end
  end

  @doc "Returns true if the argument is a call to a 'bucket' function call, false otherwise."
  @spec bucket?(t) :: boolean
  def bucket?({:function, {:bucket, _}, _}), do: true
  def bucket?(_), do: false

  @doc "Updates the bucket size argument of the given 'bucket' function with the given function call."
  @spec update_bucket_size(t, (number -> number)) :: t
  def update_bucket_size({:function, {:bucket, type}, [arg1, {:constant, size_type, size}]}, fun), do:
    {:function, {:bucket, type}, [arg1, {:constant, size_type, fun.(size)}]}

  @doc "Returns the value of the bucket size argument of the given 'bucket' function call."
  @spec bucket_size(t) :: number
  def bucket_size({:function, {:bucket, _}, [_, {:constant, _, size}]}), do: size

  @doc """
  Returns true if the function needs precompiling. Precompiling is useful if there is a costly
  preprocessing step needed that should only be done once, or if static query parameters can
  be validated prior to query execution.
  """
  @spec needs_precompiling?(String.t) :: boolean
  def needs_precompiling?(function), do: Map.get(@functions[function], :precompiled, false)

  @doc "Compiles a function so it is ready for execution"
  @spec compile_function(t, function_compilation_callback) :: t | {:error, String.t}
  def compile_function({:function, name, [_column, %Expression{value: %Regex{}}]} = precompiled_function, _callback)
    when name in ["extract_match", "extract_matches"], do: precompiled_function
  def compile_function({:function, name, [column, pattern_column]}, compilation_callback)
      when name in ["extract_match", "extract_matches"] do
    case Regex.compile(pattern_column.value, "ui") do
      {:ok, regex} ->
        regex_column = %Expression{pattern_column | value: regex}
        {:function, name, compilation_callback.([column]) ++ [regex_column]}
      {:error, {error, location}} ->
        {:error, "The regex used in `#{name}` is invalid: #{error} at character #{location}"}
    end
  end
  def compile_function({:function, function, args}, compilation_callback), do:
    {:function, function, compilation_callback.(args)}

  @doc "Returns true if the function is a valid cloak function"
  @spec exists?(t) :: boolean
  def exists?({:function, function, _}), do: @functions[function] !== nil

  @doc """
  Returns true if the function is allowed in sub-queries.

  This function expects the caller to already have validated that the function exists.
  This can be done with the `exists?` function.
  """
  @spec allowed_in_subquery?(t) :: boolean
  def allowed_in_subquery?({:function, function, _}) do
    case @functions[function] do
      %{not_in_subquery: true} -> false
      _ -> true
    end
  end

  @doc """
  Returns the first instance of a database column from a function spec
  or column. Nil if none can be found.
  """
  @spec column(t) :: Expression.t | nil
  def column(%Expression{constant?: true}), do: nil
  def column(%Expression{} = column), do: column
  def column({:function, _, args}) do
    args
    |> Enum.map(&column/1)
    |> Enum.filter(&(&1))
    |> case do
      [] -> nil
      [column|_] -> column
    end
  end


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

  defp type_matches?(type, function = {:function, _, _}), do: type_matches?(type, %{type: return_type(function)})
  defp type_matches?(type, {:distinct, column}), do: type_matches?(type, column)
  defp type_matches?({:optional, _}, nil), do: true
  defp type_matches?(_, nil), do: false
  defp type_matches?({:optional, type}, argument), do: type_matches?(type, argument)
  defp type_matches?({:or, types}, argument), do: Enum.any?(types, &type_matches?(&1, argument))
  defp type_matches?(:any, _), do: true
  defp type_matches?(_, :*), do: false
  defp type_matches?(expected_type, %{type: actual_type}), do: expected_type == actual_type

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
  defp do_apply({:bucket, :lower}, [value, bucket_size]), do:
    Float.floor(value / bucket_size) * bucket_size
  defp do_apply({:bucket, :upper}, [value, bucket_size]), do:
    Float.ceil(value / bucket_size) * bucket_size
  defp do_apply({:bucket, :middle}, [value, bucket_size]), do:
    Float.floor(value / bucket_size) * bucket_size + 0.5 * bucket_size

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
end
