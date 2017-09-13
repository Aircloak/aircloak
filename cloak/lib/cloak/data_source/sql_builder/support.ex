defmodule Cloak.DataSource.SqlBuilder.Support do
  @moduledoc "Module for detecting when a query is supported by the SQL builder module."

  alias Cloak.Sql.{Query, Expression}


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "Checks to see if the given query can be executed by the SQL driver."
  @spec supported_query?(Query.t) :: boolean
  def supported_query?(query), do:
    Query.Lenses.db_needed_functions()
    |> Lens.to_list(query)
    |> Enum.map(&function_signature/1)
    |> Enum.all?(&supported_function?(&1, Cloak.DataSource.sql_dialect_module(query.data_source)))

  @doc "Generates SQL for a function invocation. Provided arguments list must contain SQL fragments."
  @spec function_sql(Expression.function_name, [iodata], atom) :: iodata
  def function_sql("substring_for", [arg1, arg2], sql_dialect_module), do:
    function_sql("substring", [arg1, "1", arg2], sql_dialect_module)
  def function_sql({:cast, type}, [arg], sql_dialect_module), do:
    ["CAST(", arg, " AS ", sql_dialect_module.sql_type(type), ")"]
  for name <- ~w(round floor ceil ceiling trunc) do
    def function_sql(unquote(name), [arg], sql_dialect_module) do
      result = sql_dialect_module.function_sql(synonym(unquote(name)), [arg])
      function_sql({:cast, :integer}, [result], sql_dialect_module)
    end
  end
  def function_sql({:bucket, :lower}, [arg1, arg2], sql_dialect_module), do:
    # floor(arg1 / arg2) * arg2
    function_sql("*", [
      arg2,
      function_sql("floor", [
        function_sql("/", [arg1, arg2], sql_dialect_module)
      ], sql_dialect_module)
    ], sql_dialect_module)
  def function_sql({:bucket, :upper}, [arg1, arg2], sql_dialect_module), do:
    # floor(arg1 / arg2) * arg2 + arg2
    function_sql("+", [
      arg2,
      function_sql({:bucket, :lower}, [arg1, arg2], sql_dialect_module)
    ], sql_dialect_module)
  def function_sql({:bucket, :middle}, [arg1, arg2], sql_dialect_module), do:
    # floor(arg1 / arg2) * arg2 + 0.5 * arg2
    function_sql("+", [
      function_sql("*", ["0.5", arg2], sql_dialect_module),
      function_sql({:bucket, :lower}, [arg1, arg2], sql_dialect_module)
    ], sql_dialect_module)
  def function_sql(name, args, sql_dialect_module), do:
    sql_dialect_module.function_sql(synonym(name), args)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @synonyms %{
    "pow" => "^", "ceiling" => "ceil", "mod" => "%",
    "lcase" => "lower", "ucase" => "upper"
  }
  defp synonym(name), do: Map.get(@synonyms, name, name)

  defp function_signature(%Expression{function: name, function_args: args}) when is_binary(name), do:
    {name, length(args)}
  defp function_signature(%Expression{function: {:cast, _target}, function_args: [_]}), do: {"cast", 1}
  defp function_signature(%Expression{function: {:bucket, _type}, function_args: [_, _]}), do: {"bucket", 2}

  defp supported_function?({name, args}, sql_dialect_module) do
    supported_functions = sql_dialect_module.supported_functions()
    name = synonym(name)
    name in supported_functions or "#{name}/#{args}" in supported_functions
  end
end
