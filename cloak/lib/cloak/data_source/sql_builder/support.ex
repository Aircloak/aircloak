defmodule Cloak.DataSource.SqlBuilder.Support do
  @moduledoc "Module for detecting when a query is supported by the SQL builder module."

  alias Cloak.Sql.Expression

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Generates SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @spec function_sql(String.t(), [iodata], atom) :: iodata
  def function_sql(name, [arg], dialect) when name in ~w(round floor ceil trunc),
    do: name |> dialect.function_sql([arg]) |> dialect.cast_sql(:real, :integer)

  def function_sql(name, args, dialect), do: dialect.function_sql(name, args)

  @doc "Checks if the specified function can be executed by the SQL driver."
  @spec supports_function?(Expression.t(), Cloak.DataSource.t()) :: boolean
  def supports_function?(expression, data_source) do
    {name, args} = function_signature(expression)
    supported_functions = data_source.driver.sql_dialect_module().supported_functions()
    name in supported_functions or "#{name}/#{args}" in supported_functions
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp function_signature(%Expression{kind: :function, name: "count", args: [{:distinct, _arg}]}),
    do: {"count_distinct", 1}

  defp function_signature(%Expression{kind: :function, name: name, args: args}) when is_binary(name),
    do: {name, length(args)}

  defp function_signature(%Expression{kind: :function, name: {:cast, _target}, args: [_]}), do: {"cast", 1}

  defp function_signature(%Expression{kind: :function, name: {:bucket, _type}, args: [_, _]}), do: {"bucket", 2}
end
