defmodule Cloak.Sql.Compiler do
  @moduledoc "Makes the parsed SQL query ready for execution."

  alias Cloak.DataSource
  alias Cloak.Sql.{CompilationError, Expression, Query}
  alias __MODULE__

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Prepares the parsed SQL query for execution."
  @spec compile(
          Parser.parsed_query(),
          Query.analyst_id(),
          DataSource.t(),
          [Query.parameter()] | nil,
          Query.view_map()
        ) :: {:ok, Query.t()} | {:error, String.t()}
  def compile(parsed_query, analyst_id, data_source, parameters, views) do
    {:ok, compile!(parsed_query, analyst_id, data_source, parameters, views)}
  rescue
    e in CompilationError -> {:error, CompilationError.message(e)}
  end

  @doc "Prepares the parsed SQL query for execution. Raises CompilationErrors instead of returning `{:error, message}`"
  @spec compile!(
          Parser.parsed_query(),
          Query.analyst_id(),
          DataSource.t(),
          [Query.parameter()] | nil,
          Query.view_map()
        ) :: Query.t()
  def compile!(parsed_query, analyst_id, data_source, parameters, views) do
    parsed_query
    |> Compiler.Specification.compile(analyst_id, data_source, parameters, views)
    |> Compiler.Anonymization.set_query_type()
    |> Compiler.Normalization.prevalidation_normalizations()
    |> Compiler.Validation.verify_standard_restrictions()
    |> Compiler.Validation.verify_anonymization_restrictions()
    |> Compiler.TypeChecker.validate_allowed_usage_of_math_and_functions()
    |> Compiler.Execution.align()
    |> Compiler.Normalization.postvalidation_normalizations()
    |> Compiler.Optimizer.optimize()
    |> Compiler.Execution.prepare()
    |> Compiler.Optimizer.optimize_per_user_aggregation()
    |> Compiler.Anonymization.compile()
    |> Compiler.NoiseLayers.compile()
  end

  @doc "Prepares the parsed SQL query for standard (non-anonymized) execution."
  @spec compile_standard!(Parser.parsed_query(), DataSource.t()) :: Query.t()
  def compile_standard!(parsed_query, data_source),
    do:
      parsed_query
      |> Compiler.Helpers.apply_top_down(&Map.put(&1, :type, :standard))
      |> Compiler.Specification.compile(nil, data_source, nil, %{})
      |> Compiler.Normalization.prevalidation_normalizations()
      |> Compiler.Validation.verify_standard_restrictions()
      |> Compiler.Optimizer.optimize()
      |> Compiler.Execution.prepare()
      |> Compiler.Normalization.postvalidation_normalizations()

  @doc "Prepares the parsed SQL query for directly querying the data source without any processing in the cloak."
  @spec compile_direct!(Parser.parsed_query(), DataSource.t()) :: Query.t()
  def compile_direct!(parsed_query, data_source) do
    compile_standard!(parsed_query, data_source)
    |> update_in([Query.Lenses.all_queries()], fn query ->
      columns =
        query.columns
        |> Enum.zip(query.column_titles)
        |> Enum.map(fn {column, title} -> %{column | alias: title} end)

      %{query | subquery?: true, db_columns: columns}
    end)
  end

  @doc "Prepares the parsed SQL query for directly querying the data source without any processing in the cloak."
  @spec compile_direct(Parser.parsed_query(), DataSource.t()) :: {:ok, Query.t()} | {:error, String.t()}
  def compile_direct(parsed_query, data_source) do
    {:ok, compile_direct!(parsed_query, data_source)}
  rescue
    e in CompilationError -> {:error, CompilationError.message(e)}
  end

  @doc "Validates a user-defined view."
  @spec validate_view(Query.analyst_id(), DataSource.t(), Parser.parsed_query(), Query.view_map()) ::
          {:ok, Query.t()} | {:error, String.t()}
  def validate_view(analyst_id, data_source, parsed_query, views),
    do: parsed_query |> Map.put(:subquery?, true) |> compile(analyst_id, data_source, [], views)

  @doc "Creates the query which describes a SELECT statement from a single table."
  @spec make_select_query(DataSource.t(), DataSource.Table.t(), [Expression.t()]) :: Query.t()
  def make_select_query(data_source, table, select_expressions),
    do: %Query{
      command: :select,
      subquery?: true,
      columns: select_expressions,
      column_titles: Enum.map(select_expressions, &(&1.alias || &1.name)),
      from: table.name,
      data_source: data_source,
      selected_tables: [table]
    }
end
