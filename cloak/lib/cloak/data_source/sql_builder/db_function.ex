defmodule Cloak.DataSource.SqlBuilder.DbFunction do
  @moduledoc "SQL code generation for database function invocations"

  alias Cloak.DataSource.SqlBuilder.SqlBuildError


  #-----------------------------------------------------------------------------------------------------------
  # API
  #-----------------------------------------------------------------------------------------------------------

  @doc """
  Generates SQL for a function invocation.

  Provided arguments list must contain SQL fragments.
  """
  @spec sql(String.t | {:cast, Cloak.DataSource.data_type}, [iodata], Cloak.DataSource.data_type, atom) :: iodata
  def sql({:cast, type}, [arg], _parsed_type, sql_dialect),
    do: sql("cast", [arg, sql_type(type, sql_dialect)], type, sql_dialect)
  def sql(fun_name, fun_args, _parsed_type, sql_dialect), do: function_call(fun_name, fun_args, sql_dialect)


  #-----------------------------------------------------------------------------------------------------------
  # SQL generation
  #-----------------------------------------------------------------------------------------------------------

  for func <- ~w(ltrim btrim rtrim) do
    defp function_call(unquote(func), [_arg1, _arg2], :mysql),
      do: raise SqlBuildError, message: "Function #{unquote(func)} is not supported on 'mysql' data sources."
  end
  defp function_call("^", [arg1, arg2], :mysql), do: ["POW(", arg1, ", ", arg2, ")"]
  defp function_call("/", [arg1, arg2], :postgresql),  do: ["(", arg1, " :: double precision / ", arg2, ")"]
  for binary_operator <- ~w(+ - * ^ /) do
    defp function_call(unquote(binary_operator), [arg1, arg2], _sql_dialect),
      do: ["(", arg1, unquote(binary_operator), arg2, ")"]
  end
  for datepart <- ~w(year month day hour minute second) do
    defp function_call(unquote(datepart), args, _sql_dialect),
      do: ["EXTRACT(", unquote(datepart), " FROM ", args, ")"]
  end
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
  defp function_call("btrim", [arg1], :mysql), do: ["TRIM(", arg1, ")"]
  defp function_call("div", [arg1, arg2], :mysql), do: [arg1, " DIV ", arg2]
  defp function_call(name, args, _sql_dialect), do: [name, "(", Enum.intersperse(args, ", ") ,")"]

  defp sql_type(:real, :mysql), do: "decimal(65, 15)"
  defp sql_type(:real, _sql_dialect), do: "float"
  defp sql_type(:boolean, _sql_dialect), do: "bool"
  defp sql_type(:text, :mysql), do: "char"
  defp sql_type(:integer, :mysql), do: "signed"
  defp sql_type(type, _sql_dialect) when is_atom(type), do: Atom.to_string(type)
end
