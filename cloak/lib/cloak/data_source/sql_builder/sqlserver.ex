defmodule Cloak.DataSource.SqlBuilder.SQLServer do
  @moduledoc "Helper module for converting a query to SQL Server specific SQL."

  alias Cloak.Sql.Expression


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Returns the list of supported functions for this SQL dialect."
  @spec supported_functions() :: [String.t]
  def supported_functions(), do:
    ~w(
      count sum min max avg stddev
      year quarter month day hour minute second weekday
      sqrt floor ceil abs round trunc div mod ^ * / + -
      length lower upper ltrim rtrim left right substring substring_for concat
      hex cast coalesce bucket
    )

  @doc "Generates dialect specific SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @spec function_sql(Expression.function_name, [iodata]) :: iodata
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
  def function_sql("stddev", [arg]), do: ["STDEV(", arg, ")"]
  def function_sql("^", [arg1, arg2]), do: ["POWER(", arg1, ", ", arg2, ")"]
  def function_sql("/", [arg1, arg2]),  do: ["(CAST(", arg1, " AS double precision) / ", arg2, ")"]
  for binary_operator <- ~w(+ - * %) do
    def function_sql(unquote(binary_operator), [arg1, arg2]), do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  def function_sql(name, args), do: [String.upcase(name), "(", Enum.intersperse(args, ", ") ,")"]

  @doc "Returns the dialect-specific SQL type for casting."
  @spec sql_type(atom) :: String.t
  def sql_type(:real), do: "float"
  def sql_type(:boolean), do: "bool"
  def sql_type(:text), do: "char"
  def sql_type(type) when is_atom(type), do: Atom.to_string(type)
end
