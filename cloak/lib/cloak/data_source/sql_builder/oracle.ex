defmodule Cloak.DataSource.SqlBuilder.Oracle do
  @moduledoc "Helper module for converting a query to Oracle- specific SQL."

  @fmt_no_extra_whitespace "FM"

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect
  alias Cloak.DataSource.SqlBuilder.Dialect

  @impl Dialect
  def supported_functions(), do: ~w(
      count sum min max avg stddev count_distinct sum_distinct min_distinct max_distinct avg_distinct stddev_distinct
      year quarter month day hour minute second weekday date_trunc
      sqrt floor ceil abs round trunc div mod ^ % * / + -
      length lower upper btrim ltrim rtrim left right substring concat
      hex cast coalesce hash bool_op
    )

  @impl Dialect
  def function_sql("bool_op", [[?', op, ?'], arg1, arg2]) do
    condition = [arg1, " ", op, " ", arg2]
    ["(CASE WHEN ", condition, " THEN 1 WHEN NOT (", condition, ") THEN 0 ELSE NULL END)"]
  end

  for datepart <- ~w(year month day hour minute second) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("quarter", args), do: ["TRUNC((", function_sql("month", args), " - 1) / 3) + 1"]

  for binary_operator <- ~w(+ - * /) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  for {from, to} <- %{"^" => "POWER", "%" => "MOD"} do
    def function_sql(unquote(from), args), do: function_sql(unquote(to), args)
  end

  def function_sql("date_trunc", [[?', "second", ?'], arg2]), do: ["CAST(", arg2, " AS TIMESTAMP(0))"]
  def function_sql("date_trunc", [[?', "minute", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'mi'"])
  def function_sql("date_trunc", [[?', "hour", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'hh'"])
  def function_sql("date_trunc", [[?', "day", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'dd'"])
  def function_sql("date_trunc", [[?', "quarter", ?'], arg2]), do: function_sql("TRUNC", [arg2, "'q'"])
  def function_sql("date_trunc", [arg1, arg2]), do: function_sql("TRUNC", [arg2, arg1])

  def function_sql("hash", [arg]), do: ["TO_CHAR(ORA_HASH(", arg, "), '#{@fmt_no_extra_whitespace}0000000X')"]

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def cast_sql(value, :integer, :boolean),
    do: ["(CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " = 0 THEN 0 ELSE 1 END)"]

  def cast_sql(value, :real, :boolean),
    do: ["(CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " = 0.0 THEN 0 ELSE 1 END)"]

  def cast_sql(value, number, :text) when number in [:integer, :real], do: ["TO_CHAR(", value, ?)]

  def cast_sql(value, :real, :integer), do: ["ROUND(", value, ?)]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def unicode_literal(value), do: [?', value, ?']

  @impl Dialect
  def alias_sql(object, alias), do: [object, " ", alias]

  @impl Dialect
  def limit_sql(nil, 0), do: []
  def limit_sql(_, _), do: Cloak.DataSource.raise_error("Non-zero OFFSET is not natively supported on this data source")

  @impl Dialect
  def boolean_literal(true), do: ?1
  def boolean_literal(false), do: ?0

  @impl Dialect
  def native_support_for_ilike?(), do: false

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp sql_type(:real), do: "BINARY_FLOAT"
  defp sql_type(:boolean), do: "NUMBER(1)"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:integer), do: "NUMBER"
  defp sql_type(:date), do: "DATE"
  defp sql_type(:text), do: "VARCHAR2"
  defp sql_type(:time), do: "INTERVAL DAY(0) TO SECOND(6)"
end
