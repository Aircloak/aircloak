defmodule Cloak.DataSource.SqlBuilder.Drill do
  @moduledoc "Helper module for converting a query to Drill specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect
  alias Timex.Duration

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct sum_distinct min_distinct max_distinct avg_distinct stddev_distinct
      year month day hour minute second date_trunc
      sqrt floor ceil abs round trunc div mod * / + - ^ %
      length lower upper btrim ltrim rtrim left right substring concat
      cast coalesce hash bool_op
    )

  def function_sql("btrim", [arg]), do: ["TRIM(", arg, ")"]
  def function_sql("btrim", [arg1, arg2]), do: ["TRIM(", arg1, " FROM ", arg2, ")"]

  @impl Dialect
  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), arg), do: ["DATE_PART('", unquote(datepart), "', ", arg, ")"]
  end

  def function_sql("hash", [arg]), do: ["SUBSTR(MD5(", arg, "), 5, 8)"]

  def function_sql("left", [arg1, arg2]), do: ["SUBSTR(", arg1, ", 1, ", arg2, ")"]
  def function_sql("right", [arg1, arg2]), do: ["SUBSTR(", arg1, ", LENGTH(", arg1, ") - ", arg2, " + 1)"]

  def function_sql("bool_op", [[?', op, ?'], arg1, arg2]), do: ["(", arg1, " ", op, " ", arg2, ")"]

  def function_sql("/", [arg1, arg2]), do: ["(CAST(", arg1, " AS double) / ", arg2, ")"]
  def function_sql("^", [arg1, arg2]), do: ["POW(", arg1, ", ", arg2, ")"]
  def function_sql("%", [arg1, arg2]), do: ["MOD(", arg1, ", ", arg2, ")"]

  for binary_operator <- ~w(+ - *) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def ilike_sql(what, {pattern, escape = "\\"}), do: ["ILIKE(", what, ", ", ?', pattern, ?', ", ", ?', escape, ?', ")"]

  @impl Dialect
  def limit_sql(nil, 0), do: []
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def cast_sql(value, :boolean, :integer),
    do: ["CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " THEN 1 ELSE 0 END"]

  def cast_sql(value, :boolean, :real),
    do: ["CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " THEN 1.0 ELSE 0.0 END"]

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

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def unicode_literal(value), do: [?', value, ?']

  @impl Dialect
  def time_arithmetic_expression("+", [date, interval]), do: ["DATE_ADD(", date, ", ", interval, ")"]
  def time_arithmetic_expression("-", [date, interval]), do: ["DATE_SUB(", date, ", ", interval, ")"]

  @impl Dialect
  def interval_literal(interval) do
    days = Duration.to_days(interval, truncate: true)
    time = interval |> Duration.diff(Duration.from_days(days)) |> Duration.to_time!() |> to_string()
    "interval '#{days} #{time}' day(9) to second"
  end

  @impl Dialect
  def quote_char(), do: ?`

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:real), do: "double"
  defp sql_type(:boolean), do: "boolean"
  defp sql_type(:datetime), do: "timestamp"
  defp sql_type(:integer), do: "bigint"
  defp sql_type(:text), do: "varchar"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
