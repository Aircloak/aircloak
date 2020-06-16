defmodule Cloak.DataSource.SqlBuilder.ClouderaImpala do
  @moduledoc """
  Helper module for converting a query to Cloudera Impala specific SQL.
  This implementation targets Impala 2.10 of CDH 5.13.x Enterprise Edition.
  """

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  # Instead of throwing, Impala usually returns NaN and +-Infinity from functions
  # or arithmetic operations in which arguments or results exceed valid bounds.
  #
  # Because these operators/functions and inherently safe (non-crashing),
  # we forward some variants to their native implementation.
  @aliases %{
    "unsafe_add" => "+",
    "unsafe_sub" => "-",
    "unsafe_mul" => "*",
    "checked_mod" => "%",
    "unsafe_mod" => "%",
    "checked_pow" => "^"
  }

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct variance
      < > <= >= = <> and or not in is_null like ilike !<>
      year month day hour minute second quarter weekday date_trunc
      sqrt floor ceil abs round trunc mod ^ * / + - %
      unsafe_add unsafe_sub unsafe_mul unsafe_div unsafe_mod unsafe_pow
      checked_mod checked_div checked_pow
      length lower upper btrim ltrim/1 rtrim/1 left right substring concat
      hex cast coalesce case
  )

  @impl Dialect
  for {function, alias} <- @aliases do
    def function_sql(unquote(function), args), do: function_sql(unquote(alias), args)
  end

  for binary_operator <- ~w(+ - * %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql("/", [arg1, arg2]),
    do: ["CASE WHEN ", arg2, " = 0 THEN NULL ELSE (", arg1, " / ", arg2, ") END"]

  def function_sql("unsafe_div", [arg1, arg2]), do: ["(", arg1, "/", arg2, ")"]

  def function_sql("checked_div", [arg1, arg2, epsilon]),
    do: ["CASE WHEN ABS(", arg2, ") < ", epsilon, " THEN NULL ELSE (", arg1, " / ", arg2, ") END"]

  def function_sql("sqrt", [arg]), do: ["CASE WHEN ", arg, " < 0 THEN NULL ELSE SQRT(", arg, ") END"]

  def function_sql("unsafe_pow", [arg1, arg2]), do: ["POW(", arg1, ", ", arg2, ")"]

  def function_sql("^", [arg1, arg2]),
    do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE POW(", arg1, ", ", arg2, ") END"]

  # Impala rounds positive and negative numbers assymetrically (when digits < 0).
  # Therefore we ensure consistency by rounding negative numbers' absolute value.
  def function_sql("round", [arg1, arg2]),
    do: [
      "CASE WHEN ",
      arg2,
      " < 0 THEN TRUNCATE(SIGN(",
      arg1,
      ") * ROUND(ABS(",
      arg1,
      "), ",
      arg2,
      ")) ELSE ROUND(",
      arg1,
      ", ",
      arg2,
      ") END"
    ]

  def function_sql("trunc", [arg1, arg2]), do: ["TRUNCATE(CAST(", arg1, " AS DECIMAL(18, 6)), ", arg2, ")"]
  def function_sql("trunc", args), do: super("TRUNCATE", args)

  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", args, ", '", unquote(datepart), "')"]
  end

  # quarter is not supported natively in versions below Cloudera Impala 2.12 (shipped with CDH 5.15.x)
  def function_sql("quarter", args), do: ["CAST((FLOOR((EXTRACT(", args, ", 'month') - 1) / 3) + 1) AS INT)"]
  def function_sql("weekday", args), do: ["DAYOFWEEK(", args, ")"]

  def function_sql("date_trunc", [[?', "year", ?'], arg2]), do: ["TRUNC(", arg2, ", 'YY')"]
  def function_sql("date_trunc", [[?', "quarter", ?'], arg2]), do: ["TRUNC(", arg2, ", 'Q')"]
  def function_sql("date_trunc", [[?', "month", ?'], arg2]), do: ["TRUNC(", arg2, ", 'MM')"]
  def function_sql("date_trunc", [[?', "day", ?'], arg2]), do: ["TRUNC(", arg2, ", 'DD')"]
  def function_sql("date_trunc", [[?', "hour", ?'], arg2]), do: ["TRUNC(", arg2, ", 'HH')"]
  def function_sql("date_trunc", [[?', "minute", ?'], arg2]), do: ["TRUNC(", arg2, ", 'MI')"]

  def function_sql("date_trunc", [[?', "second", ?'], arg2]),
    do: ["SECONDS_ADD(TRUNC(", arg2, ", 'MI'), EXTRACT(", arg2, ", 'second'))"]

  # left of a negative value should return all but the last n characters.
  # left("aircloak", -2) --> "airclo"
  # left("aircloak", -100) --> ""
  def function_sql("left", [value, "-" <> length]) do
    substring_length =
      function_sql("unsafe_sub", [
        function_sql("length", [value]),
        length
      ])

    function_sql("case", [
      [substring_length, "> 0"],
      function_sql("substring", [value, "1", substring_length]),
      "''"
    ])
  end

  def function_sql("!<>", [arg1, arg2]), do: [arg1, " IS NOT DISTINCT FROM ", arg2]

  def function_sql("left", args), do: super("STRLEFT", args)

  # right of a negative value should return all but the first n characters
  # right("aircloak", -2) --> "rcloak"
  # right("aircloak", -100) --> ""
  def function_sql("right", [value, "-" <> length]) do
    substring_length =
      function_sql("unsafe_sub", [
        function_sql("length", [value]),
        length
      ])

    function_sql("case", [
      [substring_length, "> 0"],
      function_sql("substring", [
        value,
        function_sql("unsafe_add", [
          function_sql("abs", [length]),
          "1"
        ])
      ]),
      "''"
    ])
  end

  def function_sql("right", args), do: super("STRRIGHT", args)

  def function_sql("hex", args), do: ["LOWER(HEX(", args, "))"]

  def function_sql("case", args), do: Dialect.case_default(args)

  def function_sql("boolean_expression", [arg]), do: arg

  def function_sql(name, args), do: super(name, args)

  @impl Dialect
  def literal(%NaiveDateTime{} = value), do: [?', value |> NaiveDateTime.truncate(:millisecond) |> to_string(), ?']
  def literal(%Date{} = value), do: [?', to_string(value), ?']
  def literal(%Time{} = value), do: [?', to_string(value), ?']
  def literal(value), do: super(value)

  @impl Dialect
  def quote_char(), do: ?`

  @impl Dialect
  def limit_sql(nil, offset), do: [" LIMIT 9223372036854775807 OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def cast_sql(value, :real, :integer),
    do: ["CASE WHEN ABS(", value, ") > #{@integer_range} THEN NULL ELSE CAST(ROUND(", value, ") AS BIGINT) END"]

  def cast_sql(value, :text, :boolean),
    do: [
      "CASE WHEN TRIM(LOWER(",
      value,
      ")) IN ('1', 't', 'true', 'yes', 'y') THEN TRUE WHEN TRIM(LOWER(",
      value,
      ")) IN ('0', 'f', 'false', 'no', 'n') THEN FALSE ELSE NULL END"
    ]

  def cast_sql(value, :boolean, :text),
    do: ["CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " THEN 'true' ELSE 'false' END"]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def time_arithmetic_expression(operator, [date, interval]) when operator in ~w(+ unsafe_add),
    do: ["seconds_add(", date, ", ", interval, ")"]

  def time_arithmetic_expression(operator, [date, interval]) when operator in ~w(- unsafe_sub),
    do: ["seconds_sub(", date, ", ", interval, ")"]

  @impl Dialect
  def date_subtraction_expression(_type, [arg1, arg2]), do: ["UNIX_TIMESTAMP(", arg1, ") - UNIX_TIMESTAMP(", arg2, ")"]

  @impl Dialect
  def supports_overriding_pattern_escape?(), do: false

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # 8 byte binary float
  defp sql_type(:real), do: "DOUBLE"
  # 8 byte binary integer
  defp sql_type(:integer), do: "BIGINT"
  defp sql_type(:boolean), do: "BOOLEAN"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:date), do: "TIMESTAMP"
  defp sql_type(:time), do: "TIMESTAMP"
  defp sql_type(:text), do: "STRING"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
