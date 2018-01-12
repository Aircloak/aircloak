defmodule Cloak.Sql.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.Sql.{CompilationError, Expression, Query}
  alias __MODULE__


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(DataSource.t, Parser.parsed_query, [Query.parameter] | nil, Query.view_map) ::
    {:ok, Query.t} | {:error, String.t}
  def compile(data_source, parsed_query, parameters, views) do
    try do
      {query, features} = do_compile(data_source, parsed_query, parameters, views)
      {:ok, query, features}
    rescue
      e in CompilationError -> {:error, e.message}
    end
  end

  @doc "Validates a user-defined view."
  @spec validate_view(DataSource.t, Parser.parsed_query, Query.view_map) :: :ok | {:error, String.t}
  def validate_view(data_source, parsed_query, views), do:
    compile(data_source, Map.put(parsed_query, :subquery?, true), [], views)

  @doc "Creates the query which describes a SELECT statement from a single table."
  @spec make_select_query(DataSource.t, DataSource.Table.t, [Expression.t]) :: Query.t
  def make_select_query(data_source, table, select_expressions), do:
    data_source
    |> Compiler.Execution.make_select_query(table, select_expressions)
    |> Query.resolve_db_columns()

  defp do_compile(data_source, parsed_query, parameters, views) do
    compiled_query =
      parsed_query
      |> Compiler.ASTNormalization.normalize()
      |> Compiler.Specification.compile(data_source, parameters, views)
      |> Compiler.Validation.verify_query()
      |> Compiler.Normalization.remove_noops()
      |> Compiler.TypeChecker.validate_allowed_usage_of_math_and_functions()

    features = Query.features(compiled_query)

    final_query =
      compiled_query
      |> Compiler.Optimizer.optimize()
      |> Compiler.Execution.prepare()
      |> Compiler.Normalization.normalize()

    {final_query, %{features | emulated: final_query.emulated?}}
  end
end
