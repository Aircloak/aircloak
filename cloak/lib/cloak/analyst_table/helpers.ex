defmodule Cloak.AnalystTable.Helpers do
  @moduledoc "Helper functions for working with analyst tables."

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Compiles the analyst table select statement."
  @spec compile(String.t(), String.t(), Cloak.DataSource.t()) :: {:ok, Cloak.Sql.Query.t()} | {:error, String.t()}
  def compile(table_name, statement, data_source) do
    with :ok <- supports_analyst_tables?(data_source),
         :ok <- verify_table_name(table_name, data_source),
         {:ok, query} <- compile_statement(statement, data_source),
         :ok <- verify_query_type(query),
         :ok <- verify_offloading(query),
         do: {:ok, query}
  end

  @doc "Stores the analyst table to database."
  @spec store(any, Cloak.Sql.Query.t(), Cloak.DataSource.t()) :: {:ok, String.t()} | {:error, String.t()}
  def store(id, query, data_source),
    do: Cloak.DataSource.Connection.execute!(data_source, &data_source.driver.store_analyst_table(&1, id, query))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp supports_analyst_tables?(data_source) do
    if data_source.driver.supports_analyst_tables?(),
      do: :ok,
      else: {:error, "This data source doesn't support analyst tables."}
  end

  defp compile_statement(statement, data_source) do
    with {:ok, parsed_query} <- Cloak.Sql.Parser.parse(statement),
         {:ok, query} <- Cloak.Sql.Compiler.compile_direct(parsed_query, data_source),
         do: {:ok, query |> Cloak.Sql.Query.set_emulation_flag() |> Cloak.Sql.Compiler.Anonymization.set_query_type()}
  end

  defp verify_table_name(table_name, data_source) do
    if Enum.any?(Cloak.DataSource.tables(data_source), &(&1.name == table_name)),
      do: {:error, "The table with the given name already exists."},
      else: :ok
  end

  defp verify_query_type(query) do
    if query.type == :restricted, do: :ok, else: {:error, "At least one user id column must be selected."}
  end

  defp verify_offloading(query) do
    if query.emulated?, do: {:error, "Emulated query can't be materialized."}, else: :ok
  end
end
