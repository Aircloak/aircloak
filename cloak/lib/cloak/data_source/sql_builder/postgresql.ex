defmodule Cloak.DataSource.SqlBuilder.PostgreSQL do
  @moduledoc "Helper module for converting a query to PostgreSQL specific SQL."


  # -------------------------------------------------------------------
  # SqlBuilder.Dialect callbacks
  # -------------------------------------------------------------------

  use Cloak.DataSource.SqlBuilder.Dialect

  @doc false
  def supported_functions(), do:
    ~w(
      count sum min max avg stddev
      year quarter month day hour minute second weekday date_trunc
      sqrt floor ceil abs round trunc div mod ^ * / + -
      length lower upper btrim ltrim rtrim left right substring substring_for concat
      hex cast coalesce bucket hash
    )

  @doc false
  for datepart <- ~w(year month day hour minute second quarter) do
    def function_sql(unquote(datepart), args), do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
  def function_sql("weekday", args), do: ["EXTRACT(DOW FROM ", args, ")"]
  def function_sql("trunc", [arg1, arg2]), do: ["TRUNC(CAST(", arg1, " AS decimal), ", arg2, ")"]
  def function_sql("round", [arg1, arg2]), do: ["ROUND(CAST(", arg1, " AS decimal), ", arg2, ")"]
  def function_sql("hex", [arg]), do: ["ENCODE(", arg, "::bytea, 'hex')"]
  def function_sql("hash", [arg]), do: ["('x0' || SUBSTR(MD5(", arg, "::text), 1, 15))::bit(64)::bigint"]
  def function_sql("/", [arg1, arg2]),  do: ["(", arg1, " :: double precision / ", arg2, ")"]
  for binary_operator <- ~w(+ - * ^ %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc false
  def ilike_sql(what, match), do: [what, " ILIKE " , match]

  @doc false
  def limit_sql(nil, offset), do: [" OFFSET ", to_string(offset)]
  def limit_sql(limit, offset), do: [" LIMIT ", to_string(limit), " OFFSET ", to_string(offset)]

  @doc false
  def sql_type(:real), do: "float"
  def sql_type(:boolean), do: "bool"
  def sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
