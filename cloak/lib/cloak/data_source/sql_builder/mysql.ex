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
      count sum min max avg stddev count_distinct sum_distinct min_distinct max_distinct avg_distinct
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc div mod ^ * / + -
      length lower upper btrim/1 ltrim/1 rtrim/1 left right substring concat
      hex cast coalesce hash bool_op
    )

  @impl Dialect
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end

  def function_sql("weekday", args), do: ["(WEEKDAY(", args, ") + 1)"]
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNCATE(", arg1, ", ", arg2, ")"]
  def function_sql("trunc", [arg]), do: ["TRUNCATE(", arg, ", 0)"]
  def function_sql("btrim", [arg]), do: ["TRIM(", arg, ")"]
  def function_sql("length", [arg]), do: ["CHAR_LENGTH(", arg, ")"]
  def function_sql("div", [arg1, arg2]), do: [arg1, " DIV ", arg2]
  def function_sql("hex", [arg]), do: ["LOWER(HEX(", arg, "))"]
  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]

  def function_sql("hash", [arg]), do: ["SUBSTR(MD5(CAST(", arg, " AS char)), 5, 8)"]

  def function_sql("bool_op", [["N'", op, ?'], arg1, arg2]), do: ["(", arg1, " ", op, " ", arg2, ")"]

  def function_sql("^", [arg1, arg2]), do: ["POW(", arg1, ", ", arg2, ")"]

  for binary_operator <- ~w(+ - * / %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end

  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", "), ")"]

  @impl Dialect
  def like_sql(what, match), do: super([what, " COLLATE utf8_bin"], match)

  @impl Dialect
  def ilike_sql(what, {pattern, escape = "\\"}),
    do: [what, " COLLATE utf8_general_ci LIKE ", ?', pattern, ?', " ESCAPE ", ?', escape, ?']

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
      "CASE WHEN ",
      value,
      " IS NULL THEN NULL WHEN ",
      value,
      " = '0' THEN FALSE WHEN LOWER(",
      value,
      ") = 'false' THEN FALSE ELSE TRUE END"
    ]

  def cast_sql(value, :boolean, :text),
    do: ["CASE WHEN ", value, " IS NULL THEN NULL WHEN ", value, " THEN 'true' ELSE 'false' END"]

  def cast_sql(value, _, type), do: ["CAST(", value, " AS ", sql_type(type), ")"]

  @impl Dialect
  def unicode_literal(value), do: ["N'", value, ?']

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
