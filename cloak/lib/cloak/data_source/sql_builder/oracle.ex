defmodule Cloak.DataSource.SqlBuilder.Oracle do
  @moduledoc "Helper module for converting a query to Oracle- specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @safe_aliases %{
    "+" => "plus",
    "*" => "mul",
    "-" => "sub",
    "/" => "div",
    "^" => "pow"
  }

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev variance count_distinct
      < > <= >= = <> and or not in is_null like ilike
      year quarter month day hour minute second weekday date_trunc
      unsafe_pow unsafe_mul unsafe_div unsafe_add unsafe_sub unsafe_mod
      checked_mod checked_div checked_pow
      sqrt floor ceil abs round trunc
      length lower upper btrim ltrim rtrim left right substring concat
      hex cast coalesce grouping_id case
    )

  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("weekday", [arg]), do: cast_sql(["TO_CHAR(", arg, ", 'D')"], :text, :integer)

  def function_sql("quarter", args), do: ["TRUNC((", function_sql("month", args), " - 1) / 3) + 1"]

  def function_sql("unsafe_mod", [arg1, arg2]), do: ["MOD(", arg1, ", ", arg2, ")"]
  def function_sql("checked_mod", [arg1, arg2]), do: ["MOD(", arg1, ", NULLIF(", arg2, ", 0))"]

  def function_sql("checked_div", [arg1, arg2, epsilon]),
    do: ["CASE WHEN ", function_sql("abs", [arg2]), " < ", epsilon, " THEN NULL ELSE ", arg1, " / ", arg2, " END"]

  for {function, operator} <- %{
        "unsafe_add" => "+",
        "unsafe_sub" => "-",
        "unsafe_mul" => "*",
        "unsafe_div" => "/"
      } do
    def function_sql(unquote(function), [arg1, arg2]), do: ["(", arg1, unquote(operator), arg2, ")"]
  end

  def function_sql("unsafe_pow", [arg1, arg2]), do: ["POWER(", arg1, ", ", arg2, ")"]

  def function_sql("checked_pow", [arg1, arg2]),
    do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE POWER(", arg1, ", ", arg2, ") END"]

  def function_sql("sqrt", [arg]),
    do: ["CASE WHEN ", arg, " < 0 THEN NULL ELSE SQRT(", arg, ") END"]

  # left of a negative value should return all but the last n characters.
  # left("aircloak", -2) --> "airclo"
  # left("aircloak", -100) --> ""
  def function_sql("left", [value, "-" <> length]) do
    substring_length =
      function_sql("-", [
        function_sql("length", [value]),
        length
      ])

    [
      "CASE WHEN ",
      [substring_length, "> 0"],
      " THEN ",
      function_sql("substring", [value, "1", substring_length]),
      " ELSE ",
      "''",
      " END"
    ]
  end

  def function_sql("left", [string, number]), do: function_sql("substring", [string, "1", number])

  # right of a negative value should return all but the first n characters
  # right("aircloak", -2) --> "rcloak"
  # right("aircloak", -100) --> ""
  def function_sql("right", [value, "-" <> length]) do
    substring_length =
      function_sql("-", [
        function_sql("length", [value]),
        length
      ])

    [
      "CASE WHEN ",
      [substring_length, "> 0"],
      " THEN ",
      function_sql("substring", [
        value,
        function_sql("+", [
          function_sql("abs", [length]),
          "1"
        ])
      ]),
      " ELSE ",
      "''",
      " END"
    ]
  end

  def function_sql("right", [string, number]) do
    number =
      function_sql("least", [
        function_sql("length", [string]),
        number
      ])

    function_sql("substring", [
      string,
      ["-", number],
      number
    ])
  end

  def function_sql("hex", [data]), do: ["LOWER(RAWTOHEX(", data, "))"]

  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]

  def function_sql("variance", [arg]), do: ["VAR_SAMP(", arg, ")"]

  def function_sql("substring", args), do: function_sql("SUBSTR", args)

  def function_sql("concat", [arg]), do: arg
  def function_sql("concat", [first | rest]), do: ["CONCAT(", first, ",", function_sql("concat", rest), ")"]

  def function_sql("date_trunc", [[?', "second", ?'], arg2]), do: ["CAST(", arg2, " AS TIMESTAMP(0))"]
  def function_sql("date_trunc", [[?', "minute", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'mi'"])
  def function_sql("date_trunc", [[?', "hour", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'hh'"])
  def function_sql("date_trunc", [[?', "day", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'dd'"])
  def function_sql("date_trunc", [[?', "quarter", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'q'"])
  def function_sql("date_trunc", [arg1, arg2]), do: function_sql("TRUNC", [arg2, arg1])

  def function_sql("btrim", [arg]), do: ["TRIM(", arg, ")"]
  def function_sql("btrim", [arg1, arg2]), do: ["TRIM(", arg2, " FROM ", arg1, ")"]

  def function_sql("boolean_expression", [expression]),
    do: ["(CASE WHEN ", expression, " THEN 1 WHEN NOT (", expression, ") THEN 0 ELSE NULL END)"]

  for {operator, alias} <- @safe_aliases do
    def function_sql(unquote(operator), args), do: function_sql("aircloak.#{unquote(alias)}", args)
  end

  def function_sql("case", args), do: ["CASE", case_branches(args), " END"]

  def function_sql("ilike", [subject, [[?', pattern, ?'] | escape]]),
    do: ["(LOWER(", subject, ") LIKE LOWER('", pattern, "')", escape, ?)]

  def function_sql(name, args), do: super(name, args)

  @impl Dialect
  def cast_sql(value, :real, :integer),
    do: ["CASE WHEN ABS(", value, ") > #{@integer_range} THEN NULL ELSE CAST(", value, " AS INTEGER) END"]

  def cast_sql(value, :integer, :boolean),
    do: ["(CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " = 0 THEN 0 ELSE 1 END)"]

  def cast_sql(value, :real, :boolean),
    do: ["(CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " = 0.0 THEN 0 ELSE 1 END)"]

  def cast_sql(value, :text, :boolean),
    do: [
      "CASE WHEN TRIM(LOWER(",
      value,
      ")) IN ('1', 't', 'true', 'yes', 'y') THEN 1 WHEN TRIM(LOWER(",
      value,
      ")) IN ('0', 'f', 'false', 'no', 'n') THEN 0 ELSE NULL END"
    ]

  def cast_sql(value, number, :text) when number in [:integer, :real], do: ["TO_CHAR(", value, ")"]

  def cast_sql(value, :date, :text), do: ["TO_CHAR(", value, ", 'YYYY-MM-DD')"]
  def cast_sql(value, :datetime, :text), do: ["TO_CHAR(", value, ", 'YYYY-MM-DD HH:MI:SS AM')"]
  def cast_sql(value, :text, :date), do: ["TO_DATE(", value, ", 'YYYY-MM-DD')"]
  def cast_sql(value, :text, :datetime), do: ["TO_TIMESTAMP(", value, ", 'YYYY-MM-DD HH:MI:SS AM')"]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def alias_sql(object, alias), do: [object, " ", alias]

  @impl Dialect
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset), " ROWS"]

  def limit_sql(limit, offset), do: [" OFFSET ", to_string(offset), " ROWS FETCH NEXT ", to_string(limit), " ROWS ONLY"]

  @impl Dialect
  def literal(true), do: "1"
  def literal(false), do: "0"

  def literal(%Timex.Duration{} = duration),
    do: [
      "NUMTODSINTERVAL(",
      duration |> Timex.Duration.to_seconds() |> to_string(),
      ", 'SECOND')"
    ]

  def literal(value), do: super(value)

  @impl Dialect
  def select_table_names(prefix),
    do: "SELECT table_name FROM user_tables WHERE table_name LIKE '#{prefix}%'"

  @impl Dialect
  def analyst_meta_table_create_statement(quoted_table_name) do
    """
    CREATE TABLE #{quoted_table_name} (
      "air" VARCHAR(255) NOT NULL,
      "data_source" VARCHAR(255) NOT NULL,
      "analyst" INTEGER NOT NULL,
      "name" VARCHAR2(255) NOT NULL,
      "db_name" VARCHAR2(30) NOT NULL,
      "statement" CLOB NOT NULL,
      "fingerprint" VARCHAR2(50) NOT NULL,
      PRIMARY KEY ("air", "data_source", "analyst", "name"),
      CONSTRAINT ac_unique_name UNIQUE ("db_name")
    )
    """
  end

  @impl Dialect
  def long_string(string) do
    # Oracle doesn't support CLOB constants longer than 400 characters, so we're splitting the string into smaller
    # parts and concatenating with `||`.
    parts =
      string
      |> to_charlist()
      |> Stream.chunk_every(4000)
      |> Stream.map(&"to_clob('#{to_string(&1)}')")
      |> Enum.join(" || ")

    "(#{parts})"
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:real), do: "BINARY_DOUBLE"
  defp sql_type(:boolean), do: "NUMBER(1)"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:integer), do: "INTEGER"
  defp sql_type(:date), do: "DATE"
  defp sql_type(:text), do: "VARCHAR2(4000)"
  defp sql_type(:time), do: "TIME"
  defp sql_type({:native_type, type}), do: type

  defp case_branches([if_arg, then_arg | rest]), do: [" WHEN ", if_arg, " <> 0 THEN ", then_arg, case_branches(rest)]
  defp case_branches([else_branch]), do: [" ELSE ", else_branch]
end
