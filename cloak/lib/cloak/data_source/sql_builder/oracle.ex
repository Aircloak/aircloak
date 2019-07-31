defmodule Cloak.DataSource.SqlBuilder.Oracle do
  @moduledoc "Helper module for converting a query to Oracle- specific SQL."

  @fmt_no_extra_whitespace "FM"
  @unicode_substring "SUBSTRC"

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.Query.ExecutionError

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev variance count_distinct sum_distinct min_distinct max_distinct avg_distinct
      year quarter month day hour minute second weekday date_trunc
      unsafe_pow unsafe_mul unsafe_div unsafe_add unsafe_sub unsafe_mod
      checked_mod checked_div checked_pow
      sqrt floor ceil abs round trunc
      length lower upper btrim ltrim rtrim left right substring concat
      hex cast coalesce hash bool_op grouping_id
    )

  @impl Dialect
  def select_hints() do
    "/*+ parallel (8) */ "
  end

  @impl Dialect
  def function_sql("bool_op", [[?', op, ?'], arg1, arg2]) do
    condition = [arg1, " ", op, " ", arg2]
    ["(CASE WHEN ", condition, " THEN 1 WHEN NOT (", condition, ") THEN 0 ELSE NULL END)"]
  end

  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("quarter", args), do: ["TRUNC((", function_sql("month", args), " - 1) / 3) + 1"]

  def function_sql("unsafe_mod", [arg1, arg2]), do: ["MOD(", arg1, ", ", arg2, ")"]
  def function_sql("checked_mod", [arg1, arg2]), do: ["MOD(", arg1, ", NULLIF(", arg2, ", 0))"]

  def function_sql("checked_div", [arg1, arg2, epsilon]),
    do: ["CASE WHEN ", function_sql("abs", [arg2]), " < ", epsilon, " THEN NULL ELSE ", arg1, " / ", arg2, " END"]

  for {function, operator} <- %{"unsafe_add" => "+", "unsafe_sub" => "-", "unsafe_mul" => "*", "unsafe_div" => "/"} do
    def function_sql(unquote(function), [arg1, arg2]), do: ["(", arg1, unquote(operator), arg2, ")"]
  end

  def function_sql("unsafe_pow", [arg1, arg2]), do: ["POWER(", arg1, ", ", arg2, ")"]

  def function_sql("checked_pow", [arg1, arg2]),
    do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE POWER(", arg1, ", ", arg2, ") END"]

  def function_sql("sqrt", [arg]), do: ["CASE WHEN ", arg, " < 0 THEN NULL ELSE SQRT(", arg, ") END"]

  def function_sql("left", [string, number]), do: [@unicode_substring, "(", string, ", 0, ", number, ")"]

  def function_sql("right", [string, number]) do
    number = ["LEAST(LENGTHC(", string, "), ", number, ")"]
    [@unicode_substring, "(", string, ", -", number, ", ", number, ")"]
  end

  def function_sql("hex", [data]), do: ["LOWER(RAWTOHEX(", data, "))"]

  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]

  def function_sql("variance", [arg]), do: ["VAR_SAMP(", arg, ")"]

  def function_sql("substring", args), do: function_sql(@unicode_substring, args)

  def function_sql("date_trunc", [[?', "second", ?'], arg2]), do: ["CAST(", arg2, " AS TIMESTAMP(0))"]
  def function_sql("date_trunc", [[?', "minute", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'mi'"])
  def function_sql("date_trunc", [[?', "hour", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'hh'"])
  def function_sql("date_trunc", [[?', "day", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'dd'"])
  def function_sql("date_trunc", [[?', "quarter", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'q'"])
  def function_sql("date_trunc", [arg1, arg2]), do: function_sql("TRUNC", [arg2, arg1])

  def function_sql("btrim", [arg]), do: ["TRIM(", arg, ")"]
  def function_sql("btrim", [arg1, arg2]), do: ["TRIM(", arg1, " FROM ", arg2, ")"]

  def function_sql("hash", [arg]), do: ["TO_CHAR(ORA_HASH(", arg, "), '#{@fmt_no_extra_whitespace}0000000X')"]

  def function_sql("*", args), do: function_sql("aircloak.mul", args)

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

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

  def cast_sql(value, number, :text) when number in [:integer, :real], do: ["TO_CHAR(", value, ?)]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def alias_sql(object, alias), do: [object, " ", alias]

  @impl Dialect
  def limit_sql(nil, 0), do: []
  def limit_sql(limit, 0), do: [" WHERE ROWNUM <= ", to_string(limit)]

  def limit_sql(_, _),
    do: raise(ExecutionError, message: "Non-zero OFFSET is not natively supported on this data source")

  @impl Dialect
  def literal(true), do: "1"
  def literal(false), do: "0"

  def literal(%Timex.Duration{} = duration),
    do: ["NUMTODSINTERVAL(", duration |> Timex.Duration.to_seconds() |> to_string(), ", 'SECOND')"]

  def literal(value), do: Dialect.literal_default(value)

  @impl Dialect
  def native_support_for_ilike?(), do: false

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
  defp sql_type(:text), do: "VARCHAR2"
  defp sql_type(:time), do: "TIME"
  defp sql_type({:native_type, type}), do: type
end
