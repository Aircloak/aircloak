defmodule Cloak.DataSource.SqlBuilder.MySQL do
  @moduledoc "Helper module for converting a query to MySQl/MariaDB specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @max_unsigned_bigint 18_446_744_073_709_551_615

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev variance count_distinct
      < > <= >= = <> and or not in is_null like ilike !<>
      year quarter month day hour minute second weekday
      unsafe_pow unsafe_mul unsafe_div unsafe_add unsafe_sub unsafe_mod
      checked_mod checked_div checked_pow
      sqrt floor ceil abs round trunc
      length lower upper btrim/1 ltrim/1 rtrim/1 left right substring concat
      hex cast coalesce case
    )

  @impl Dialect
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("weekday", args), do: ["DAYOFWEEK(", args, ")"]
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNCATE(", arg1, ", ", arg2, ")"]
  def function_sql("trunc", [arg]), do: ["TRUNCATE(", arg, ", 0)"]
  def function_sql("btrim", [arg]), do: ["TRIM(", arg, ")"]
  def function_sql("length", [arg]), do: ["CHAR_LENGTH(", arg, ")"]
  def function_sql("hex", [arg]), do: ["LOWER(HEX(", arg, "))"]
  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]
  def function_sql("variance", [arg]), do: ["VAR_SAMP(", arg, ")"]

  def function_sql("boolean_expression", [arg]), do: arg

  def function_sql("checked_mod", [arg1, arg2]), do: ["(", arg1, " % NULLIF(", arg2, ", 0))"]

  def function_sql("checked_div", [arg1, arg2, epsilon]),
    do: ["CASE WHEN ", function_sql("abs", [arg2]), " < ", epsilon, " THEN NULL ELSE ", arg1, " / ", arg2, " END"]

  for {function, operator} <- %{
        "unsafe_add" => "+",
        "unsafe_sub" => "-",
        "unsafe_mul" => "*",
        "unsafe_div" => "/",
        "unsafe_mod" => "%"
      } do
    def function_sql(unquote(function), [arg1, arg2]), do: ["(", arg1, unquote(operator), arg2, ")"]
  end

  def function_sql("unsafe_pow", [arg1, arg2]), do: ["POW(", arg1, ", ", arg2, ")"]

  def function_sql("checked_pow", [arg1, arg2]),
    do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE POW(", arg1, ", ", arg2, ") END"]

  def function_sql("case", args), do: Dialect.case_default(args)

  def function_sql("like", [subject, pattern]), do: [?(, subject, " COLLATE utf8_bin LIKE ", pattern, ?)]
  def function_sql("ilike", [subject, pattern]), do: [?(, subject, " COLLATE utf8_general_ci LIKE ", pattern, ?)]

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

  def function_sql("!<>", [arg1, arg2]), do: [arg1, " <=> ", arg2]

  def function_sql(name, args), do: super(name, args)

  @impl Dialect
  def limit_sql(nil, offset), do: [" LIMIT ", to_string(offset), ", #{@max_unsigned_bigint}"]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(offset), ", ", to_string(limit)]

  @impl Dialect
  def cast_sql(value, :integer, :boolean),
    do: ["CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " = 0 THEN FALSE ELSE TRUE END"]

  def cast_sql(value, :real, :boolean),
    do: [
      "CASE WHEN ",
      value,
      " IS NULL THEN NULL WHEN ",
      value,
      " = 0.0 THEN FALSE ELSE TRUE END"
    ]

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
  def literal(value) when is_binary(value), do: ["N'", value, ?']
  def literal(value), do: super(value)

  @impl Dialect
  def order_by(column, :asc, :nulls_last), do: [column, " IS NULL, ", column, " ASC"]
  def order_by(column, :desc, :nulls_first), do: [column, " IS NOT NULL, ", column, " DESC"]
  def order_by(column, :asc, _), do: [column, " ASC"]
  def order_by(column, :desc, _), do: [column, " DESC"]

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:real), do: "decimal(65, 15)"
  defp sql_type(:text), do: "char"
  defp sql_type(:integer), do: "signed"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
