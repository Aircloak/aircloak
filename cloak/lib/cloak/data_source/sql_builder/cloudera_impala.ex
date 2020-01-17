defmodule Cloak.DataSource.SqlBuilder.ClouderaImpala do
  @moduledoc "Helper module for converting a query to Cloudera Data Platform (CDP) Impala specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @unsafe_operators %{
    "unsafe_add" => "+",
    "unsafe_sub" => "-",
    "unsafe_mul" => "*",
    "unsafe_div" => "/",
    "unsafe_mod" => "%"
  }

  # ILIKE requires CDH 5.7 / Impala 2.5 and higher

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct variance
      < > <= >= = <> and or not in is_null like ilike
      year month day hour minute second quarter weekday date_trunc
      sqrt floor ceil abs round trunc
      unsafe_pow unsafe_add unsafe_sub unsafe_mul unsafe_div unsafe_mod
      checked_mod checked_div checked_pow
      length lower upper btrim ltrim/1 rtrim/1 left right
      hex coalesce
  )

  @impl Dialect
  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", args, ", '", unquote(datepart), "')"]
  end

  # quarter is not supported natively in CDP 5.13
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

  def function_sql("unsafe_pow", [arg1, arg2]), do: ["POW(", arg1, ", ", arg2, ")"]

  for {function, operator} <- @unsafe_operators do
    def function_sql(unquote(function), [arg1, arg2]), do: ["(", arg1, unquote(operator), arg2, ")"]
  end

  def function_sql("sqrt", [arg]), do: ["CASE WHEN ", arg, " < 0 THEN NULL ELSE SQRT(", arg, ") END"]

  def function_sql("checked_mod", [arg1, arg2]), do: ["MOD(", arg1, ", NULLIF(", arg2, ", 0))"]

  def function_sql("checked_div", [arg1, arg2, epsilon]),
    do: ["CASE WHEN ABS(", arg2, ") < ", epsilon, " THEN NULL ELSE (", arg1, " / ", arg2, ") END"]

  def function_sql("checked_pow", [arg1, arg2]),
    do: ["CASE WHEN ", arg1, " < 0 THEN NULL ELSE POW(", arg1, ", ", arg2, ") END"]

  def function_sql("trunc", args), do: super("TRUNCATE", args)

  def function_sql("left", args), do: super("STRLEFT", args)
  def function_sql("right", args), do: super("STRRIGHT", args)

  def function_sql(name, args), do: super(name, args)

  @impl Dialect
  def literal(value), do: super(value)

  @impl Dialect
  def quote_char(), do: ?`

  @impl Dialect
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @impl Dialect
  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

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
  defp sql_type(:text), do: "STRING"
  defp sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
