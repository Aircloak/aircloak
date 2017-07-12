defmodule Cloak.DataSource.SqlBuilder.DbFunction do
  @moduledoc "SQL code generation for database function invocations"

  alias Cloak.Query.ExecutionError


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Generates SQL for a function invocation.

  Provided arguments list must contain SQL fragments.
  """
  @spec sql(Cloak.Sql.Expression.function_name, [iodata], Cloak.DataSource.data_type, atom) :: iodata
  def sql({:cast, type}, [arg], _parsed_type, sql_dialect),
    do: sql("cast", [arg, sql_type(type, sql_dialect)], type, sql_dialect)
  def sql(fun_name, fun_args, _parsed_type, sql_dialect), do: function_call(fun_name, fun_args, sql_dialect)


  # -------------------------------------------------------------------
  # SQL generation
  # -------------------------------------------------------------------

  for func <- ~w(ltrim btrim rtrim) do
    defp function_call(unquote(func), [_arg1, _arg2], :mysql),
      do: raise ExecutionError, message: "Function #{unquote(func)} is not supported on 'mysql' data sources."
  end
  defp function_call("^", [arg1, arg2], :sqlserver), do: ["POWER(", arg1, ", ", arg2, ")"]
  defp function_call("^", [arg1, arg2], :mysql), do: ["POW(", arg1, ", ", arg2, ")"]
  defp function_call("/", [arg1, arg2], :postgresql),  do: ["(", arg1, " :: double precision / ", arg2, ")"]
  for binary_operator <- ~w(+ - * ^ / %) do
    defp function_call(unquote(binary_operator), [arg1, arg2], _sql_dialect),
      do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  for datepart <- ~w(year month day hour minute second) do
    defp function_call(unquote(datepart), args, :sqlserver),
      do: ["DATEPART(", unquote(datepart), ", ", args, ")"]
    defp function_call(unquote(datepart), args, _sql_dialect),
      do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
  defp function_call("ceiling", [arg], :sqlserver), do: ["ceiling(", arg, ")"]
  for {fun, delegate_to} <- %{
    "ceiling" => "ceil",
    "lcase" => "lower",
    "ucase" => "upper",
    "||" => "concat"
  } do
    defp function_call(unquote(fun), args, sql_dialect),
      do: function_call(unquote(delegate_to), args, sql_dialect)
  end
  defp function_call("substring_for", [arg1, arg2], sql_dialect),
    do: function_call("substring", [arg1, "1", arg2], sql_dialect)
  defp function_call("cast", [arg1, arg2], _sql_dialect),
    do: ["CAST(", arg1, " AS ", arg2, ")"]
  defp function_call("trunc", [arg1, arg2], :mysql), do: ["TRUNCATE(", arg1, ", ", arg2, ")"]
  defp function_call("trunc", [arg1], :mysql), do: ["TRUNCATE(", arg1, ", 0)"]
  defp function_call("trunc", [arg1, arg2], :postgresql), do: ["TRUNC(CAST(", arg1, "AS DECIMAL), ", arg2, ")"]
  defp function_call("round", [arg1, arg2], :postgresql), do: ["ROUND(CAST(", arg1, "AS DECIMAL), ", arg2, ")"]
  defp function_call("btrim", [arg1], :mysql), do: ["TRIM(", arg1, ")"]
  defp function_call("div", [arg1, arg2], :mysql), do: [arg1, " DIV ", arg2]
  defp function_call("hex", [arg], :postgresql), do: ["ENCODE(", arg, "::bytea, 'hex')"]
  defp function_call("hex", [arg], :sqlserver), do: ["CONVERT(nvarchar, CAST(", arg, " AS varbinary), 2)"]
  defp function_call("stddev", [arg], :sqlserver), do: ["STDEV(", arg, ")"]
  defp function_call({:bucket, :lower}, [arg1, arg2], sql_dialect), do:
    # floor(arg1 / arg2) * arg2
    function_call("*", [
      arg2,
      function_call("floor", [
        function_call("/", [arg1, arg2], sql_dialect)
      ], sql_dialect)
    ], sql_dialect)
  defp function_call({:bucket, :upper}, [arg1, arg2], sql_dialect), do:
    # floor(arg1 / arg2) * arg2 + arg2
    function_call("+", [
      arg2,
      function_call({:bucket, :lower}, [arg1, arg2], sql_dialect)
    ], sql_dialect)
  defp function_call({:bucket, :middle}, [arg1, arg2], sql_dialect), do:
    # floor(arg1 / arg2) * arg2 + 0.5 * arg2
    function_call("+", [
      function_call("*", ["0.5", arg2], sql_dialect),
      function_call({:bucket, :lower}, [arg1, arg2], sql_dialect)
    ], sql_dialect)
  @noise_aggregates ["count_noise", "sum_noise", "avg_noise", "stddev_noise"]
  defp function_call(name, _args, _sql_dialect) when name in @noise_aggregates,
    do: raise ExecutionError, "Aggregation functions for noise estimation are not allowed in sub-queries!"
  defp function_call(name, args, _sql_dialect), do: [name, "(", Enum.intersperse(args, ", ") ,")"]

  defp sql_type(:real, :mysql), do: "decimal(65, 15)"
  defp sql_type(:real, _sql_dialect), do: "float"
  defp sql_type(:boolean, _sql_dialect), do: "bool"
  defp sql_type(:text, :mysql), do: "char"
  defp sql_type(:integer, :mysql), do: "signed"
  defp sql_type(:text, :sqlserver), do: "char"
  defp sql_type(type, _sql_dialect) when is_atom(type), do: Atom.to_string(type)
end
