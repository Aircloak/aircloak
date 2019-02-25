defmodule Cloak.Sql.Compiler.AnalystTables do
  @moduledoc "Compilation of analyst tables."

  alias Cloak.Sql.Query
  alias Cloak.Sql.Query.Lenses

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Replaces subqueries which represent analyst tables with names of the tables in the database."
  @spec compile(Query.t()) :: Query.t()
  def compile(query), do: Cloak.Sql.Compiler.Helpers.apply_top_down(query, &replace_analyst_tables_subqueries/1)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp replace_analyst_tables_subqueries(query) do
    required_analyst_tables = required_analyst_tables(query)

    # replace each analyst table subquery with the string reference to the table by the subquery alias
    query = Lens.map(Lenses.analyst_tables_subqueries(), query, fn {:subquery, %{alias: alias}} -> alias end)

    # replace each selected_table corresponding to an analyst table subquery with proper analyst table definition
    selected_tables = Enum.map(query.selected_tables, &Map.get(required_analyst_tables, &1.name, &1))

    # include required analyst tables in the list of available tables
    available_tables = query.available_tables ++ Map.values(required_analyst_tables)

    %Query{query | available_tables: available_tables, selected_tables: selected_tables}
  end

  defp required_analyst_tables(query) do
    query
    |> get_in([Lenses.analyst_tables_subqueries()])
    |> Stream.map(fn {:subquery, %{ast: %{analyst_table: table}, alias: alias}} -> {alias, table} end)
    |> Stream.map(fn {alias, table} -> {alias, analyst_table_definition(query, table, alias)} end)
    |> Map.new()
  end

  defp analyst_table_definition(query, table, alias) do
    with :ok <- check_table_status(query, table),
         {:ok, table_def} <- Cloak.AnalystTable.to_cloak_table(table, query.views, name: alias),
         do: table_def,
         else: ({:error, reason} -> report_error(table, reason))
  end

  defp check_table_status(query, table) do
    case Cloak.AnalystTable.find(query.analyst_id, table.name, query.data_source).status do
      :creating -> {:error, "the table is still being created"}
      :create_error -> {:error, "the table wasn't successfully created"}
      :created -> :ok
    end
  end

  defp report_error(table, reason),
    do: raise(Cloak.Sql.CompilationError, message: "Error in analyst table #{table.name}: #{reason}")
end
