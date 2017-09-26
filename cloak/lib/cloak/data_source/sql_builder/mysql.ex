defmodule Cloak.DataSource.SqlBuilder.MySQL do
  @moduledoc "Helper module for converting a query to MySQl/MariaDB specific SQL."


  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect

  @max_unsigned_bigint 18_446_744_073_709_551_615

  @doc false
  def supported_functions(), do:
    ~w(
      count sum min max avg stddev count_distinct sum_distinct min_distinct max_distinct avg_distinct
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc div mod ^ * / + -
      length lower upper btrim/1 ltrim/1 rtrim/1 left right substring substring_for concat
      hex cast coalesce hash
    )

  @doc false
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
  def function_sql("weekday", args), do: ["(WEEKDAY(", args, ") + 1)"]
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNCATE(", arg1, ", ", arg2, ")"]
  def function_sql("trunc", [arg1]), do: ["TRUNCATE(", arg1, ", 0)"]
  def function_sql("btrim", [arg1]), do: ["TRIM(", arg1, ")"]
  def function_sql("length", [arg1]), do: ["CHAR_LENGTH(", arg1, ")"]
  def function_sql("div", [arg1, arg2]), do: [arg1, " DIV ", arg2]
  def function_sql("hex", [arg]), do: ["LOWER(HEX(", arg, "))"]
  def function_sql("stddev", [arg]), do: ["STDDEV_SAMP(", arg, ")"]
  def function_sql("hash", [arg]), do: ["CAST(CONV(SUBSTR(MD5(CAST(", arg, " AS char)), 1, 15), 16, 10) AS signed)"]
  def function_sql("^", [arg1, arg2]), do: ["POW(", arg1, ", ", arg2, ")"]
  for binary_operator <- ~w(+ - * / %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc false
  def like_sql(what, match), do: super([what, " COLLATE utf8_bin"], match)

  @doc false
  def ilike_sql(what, match), do: [what, " COLLATE utf8_general_ci LIKE " , match]

  @doc false
  def limit_sql(nil, offset), do: [" LIMIT ", to_string(offset), ", #{@max_unsigned_bigint}"]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(offset), ", ", to_string(limit)]

  @doc false
  def sql_type(:real), do: "decimal(65, 15)"
  def sql_type(:boolean), do: "bool"
  def sql_type(:text), do: "char"
  def sql_type(:integer), do: "signed"
  def sql_type(type) when is_atom(type), do: Atom.to_string(type)

  @doc false
  def unicode_literal(value), do: ["N'", value, ?']
end
