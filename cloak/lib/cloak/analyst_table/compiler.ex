defmodule Cloak.AnalystTable.Compiler do
  @moduledoc "Compilation of analyst table query."

  alias Cloak.Sql.{Compiler, Query}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Compiles the analyst table select statement."
  @spec compile(
          String.t(),
          String.t(),
          Query.analyst_id(),
          Cloak.DataSource.t(),
          [Query.parameters()] | nil,
          Query.view_map()
        ) :: {:ok, Query.t()} | {:error, String.t()}
  def compile(table_name, statement, analyst, data_source, parameters, views) do
    with :ok <- verify_table_name(table_name, data_source),
         {:ok, query} <- compile_statement(statement, analyst, data_source, parameters, views),
         :ok <- verify_cycle(query, table_name),
         :ok <- verify_query_type(query),
         :ok <- verify_offloading(query),
         :ok <- verify_selected_columns(query),
         do: {:ok, query}
  end

  defp verify_cycle(query, table_name) do
    if MapSet.member?(query.required_analyst_tables, table_name),
      do: {:error, "circular dependencies between analyst tables are not supported"},
      else: :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_statement(statement, analyst, data_source, parameters, views) do
    with :ok <- naive_compilation(statement, analyst, data_source, parameters, views),
         {:ok, query} <- compile_analyst_table(statement, analyst, data_source, parameters, views),
         do: {:ok, query |> Query.set_emulation_flag() |> Cloak.Sql.Compiler.Anonymization.set_query_type()}
  end

  defp naive_compilation(statement, analyst, data_source, parameters, views) do
    # For exact error reporting, we're compiling the query directly. This will help us properly report parser error
    # location, as well as verify the query semantics, such as uid selection.
    with {:ok, parsed_query} <- Cloak.Sql.Parser.parse(statement),
         {:ok, query} <- Compiler.compile_direct(parsed_query, analyst, data_source, parameters, views),
         query = query |> Query.set_emulation_flag() |> Cloak.Sql.Compiler.Anonymization.set_query_type(),
         :ok <- verify_query_type(query),
         :ok <- verify_offloading(query),
         :ok <- verify_selected_columns(query),
         do: :ok
  end

  defp compile_analyst_table(statement, analyst, data_source, parameters, views) do
    # This is the real compilation of the analyst table query. Here, we're compiling the anonymized query
    # `select * from #{analyst_query}` to enforce aircloak restrictions, such as range alignments.
    with {:ok, parsed_query} <- Cloak.Sql.Parser.parse("select * from (#{statement}) sq") do
      query = Compiler.core_compile!(parsed_query, analyst, data_source, parameters, views)
      {:subquery, %{ast: subquery}} = query.from
      {:ok, Query.resolve_db_columns(subquery)}
    end
  rescue
    e in Cloak.Sql.CompilationError -> {:error, Cloak.Sql.CompilationError.message(e)}
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

  defp verify_selected_columns(query) do
    (query.column_titles -- Enum.uniq(query.column_titles))
    |> Enum.uniq()
    |> case do
      [] -> :ok
      duplicates -> {:error, "Duplicate column names: #{duplicates |> Stream.map(&"`#{&1}`") |> Enum.join(", ")}"}
    end
  end
end
