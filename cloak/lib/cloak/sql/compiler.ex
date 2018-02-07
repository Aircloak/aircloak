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
    {:ok, Query.t, Query.features} | {:error, String.t}
  def compile(data_source, parsed_query, parameters, views) do
    {query, features} = compile!(data_source, parsed_query, parameters, views)
    {:ok, query, features}
  rescue
    e in CompilationError -> {:error, CompilationError.message(e)}
  end

  @doc "Prepares the parsed SQL query for execution. Raises CompilationErrors instead of returning `{:error, message}`"
  @spec compile!(DataSource.t, Parser.parsed_query, [Query.parameter] | nil, Query.view_map) ::
    {Query.t, Query.features}
  def compile!(data_source, parsed_query, parameters, views) do
    compiled_query =
      parsed_query
      |> Compiler.ASTNormalization.normalize()
      |> Compiler.Specification.compile(data_source, parameters, views)
      |> Compiler.Normalization.remove_noops()
      |> Compiler.Validation.verify_query()
      |> Compiler.TypeChecker.validate_allowed_usage_of_math_and_functions()

    features = Query.features(compiled_query)

    final_query =
      compiled_query
      |> Compiler.Optimizer.optimize()
      |> Compiler.Execution.prepare()
      |> Compiler.Normalization.normalize()
      |> Query.set_emulation_flag()

    {final_query, %{features | emulated: final_query.emulated?}}
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
end
