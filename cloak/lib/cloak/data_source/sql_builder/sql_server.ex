defmodule Cloak.DataSource.SqlBuilder.SQLServer do
  @moduledoc "Helper module for converting a query to SQL Server specific SQL."

  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect

  @doc false
  def supported_functions(), do:
    ~w(
      count sum min max avg stddev
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc div mod ^ * / + -
      length lower upper ltrim rtrim left right substring substring_for concat
      hex cast coalesce bucket hash
    )

  @doc false
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["DATEPART(", unquote(datepart), ", ", args, ")"]
  end
  def function_sql("ceil", [arg]), do: ["CEILING(", arg, ")"]
  def function_sql("concat", args), do: Enum.intersperse(args, " + ")
  def function_sql("length", [arg]), do: ["LEN(", arg, ")"]
  def function_sql("trunc", [arg1]), do: ["ROUND(", arg1, ", 0, 1)"]
  def function_sql("trunc", [arg1, arg2]), do: ["ROUND(", arg1, ",", arg2, ", 1)"]
  def function_sql("round", [arg1]), do: ["ROUND(", arg1, ", 0)"]
  def function_sql("div", [arg1, arg2]), do: ["(", arg1, " / ", arg2, ")"]
  def function_sql("hex", [arg]), do: ["CONVERT(nvarchar, CAST(", arg, " AS varbinary), 2)"]
  def function_sql("hash", [arg]), do:
    ["CONVERT(bigint, SUBSTRING(0x00 + HASHBYTES('md5', CAST(", arg, " AS binary)), 1, 8))"]
  def function_sql("stddev", [arg]), do: ["STDEV(", arg, ")"]
  def function_sql("^", [arg1, arg2]), do: ["POWER(", arg1, ", ", arg2, ")"]
  def function_sql("/", [arg1, arg2]),  do: ["(CAST(", arg1, " AS double precision) / ", arg2, ")"]
  for binary_operator <- ~w(+ - * %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc false
  def like_sql(what, match), do: super([what, " COLLATE Latin1_General_CS_AS"], match)

  @doc false
  def ilike_sql(what, match), do: [what, " COLLATE Latin1_General_CI_AS LIKE " , match]

  @doc false
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset), " ROWS"]
  def limit_sql(limit, offset), do: [" OFFSET ", to_string(offset), " ROWS FETCH NEXT ", to_string(limit), " ROWS ONLY"]

  @doc false
  def cast_unknown_sql(column_sql), do:
    # We can't directly select a field with an unknown type, so convert it to binary
    # This is needed in the case of using the ODBC driver with a GUID user id,
    # as the GUID type is not supported by the Erlang ODBC library
    Cloak.DataSource.SqlBuilder.Support.function_sql({:cast, :varbinary}, [column_sql], __MODULE__)

  @doc false
  def sql_type(:real), do: "float"
  def sql_type(:boolean), do: "bool"
  def sql_type(:text), do: "varchar(max)"
  def sql_type(type) when is_atom(type), do: Atom.to_string(type)

  @doc false
  def unicode_literal(value), do: ["N'", value, ?']
end
