defmodule Cloak.DataSource.SqlBuilder.MySQL do
  @moduledoc "Helper module for converting a query to MySQl/MariaDB specific SQL."


  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  @behaviour Cloak.DataSource.SqlBuilder.Dialect

  @doc false
  def supported_functions(), do:
    ~w(
      count sum min max avg stddev
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc div mod ^ * / + -
      length lower upper btrim/1 ltrim/1 rtrim/1 left right substring substring_for concat
      hex cast coalesce bucket hash
    )

  @doc false
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNCATE(", arg1, ", ", arg2, ")"]
  def function_sql("trunc", [arg1]), do: ["TRUNCATE(", arg1, ", 0)"]
  def function_sql("btrim", [arg1]), do: ["TRIM(", arg1, ")"]
  def function_sql("div", [arg1, arg2]), do: [arg1, " DIV ", arg2]
  def function_sql("hash", [arg]), do: ["CAST(CONV(SUBSTR(MD5(CAST(", arg, " AS char)), 1, 15), 16, 10) AS signed)"]
  def function_sql("^", [arg1, arg2]), do: ["POW(", arg1, ", ", arg2, ")"]
  for binary_operator <- ~w(+ - * / %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc false
  def sql_type(:real), do: "decimal(65, 15)"
  def sql_type(:boolean), do: "bool"
  def sql_type(:text), do: "char"
  def sql_type(:integer), do: "signed"
  def sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
