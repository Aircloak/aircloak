defmodule Cloak.DataSource.SqlBuilder.Support do
  @moduledoc "Module for detecting when a query is supported by the SQL builder module."

  alias Cloak.Sql.Expression


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Generates SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @spec function_sql(Expression.function_name | {:cast, atom, atom}, [iodata], atom) :: iodata
  def function_sql({:cast, from_type, to_type}, [arg], sql_dialect_module), do:
    sql_dialect_module.cast_sql(arg, from_type, to_type)
  for name <- ~w(round floor ceil ceiling trunc) do
    def function_sql(unquote(name), [arg], sql_dialect_module) do
      unquote(name)
      |> synonym()
      |> sql_dialect_module.function_sql([arg])
      |> sql_dialect_module.cast_sql(:real, :integer)
    end
  end
  def function_sql(name, args, sql_dialect_module), do:
    sql_dialect_module.function_sql(synonym(name), args)

  @doc "Checks if the specified function can be executed by the SQL driver."
  @spec supports_function?(Expression.t, Cloak.DataSource.t) :: boolean
  def supports_function?(expression, data_source) do
    {name, args} = function_signature(expression)
    supported_functions = Cloak.DataSource.sql_dialect_module(data_source).supported_functions()
    name = synonym(name)
    name in supported_functions or "#{name}/#{args}" in supported_functions
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @synonyms %{
    "pow" => "^", "ceiling" => "ceil", "mod" => "%",
    "lcase" => "lower", "ucase" => "upper"
  }
  defp synonym(name), do: Map.get(@synonyms, name, name)

  defp function_signature(%Expression{function: name, function_args: [{:distinct, _arg}]}) when is_binary(name), do:
    {name <> "_distinct", 1}
  defp function_signature(%Expression{function: name, function_args: args}) when is_binary(name), do:
    {name, length(args)}
  defp function_signature(%Expression{function: {:cast, _target}, function_args: [_]}), do: {"cast", 1}
  defp function_signature(%Expression{function: {:bucket, _type}, function_args: [_, _]}), do: {"bucket", 2}
end
