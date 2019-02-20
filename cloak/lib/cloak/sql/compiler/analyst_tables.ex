defmodule Cloak.Sql.Compiler.AnalystTables do
  @moduledoc "Compilation of analyst tables."

  alias Cloak.Sql.{Expression, Query}
  alias Cloak.Sql.Query.Lenses
  alias Cloak.Sql.Compiler.Helpers

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Replaces subqueries which represent analyst tables with names of the tables in the database."
  @spec compile(Query.t()) :: Query.t()
  def compile(query), do: Helpers.apply_top_down(query, &replace_analyst_tables_subqueries/1)

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
         {:ok, table_query} <- compile_analyst_table(query, table),
         do: table_definition(table, table_query, alias),
         else: ({:error, reason} -> report_error(table, reason))
  end

  defp check_table_status(query, table) do
    case Cloak.AnalystTable.find(query.analyst_id, table.name, query.data_source).status do
      :creating -> {:error, "the table is still being created"}
      :create_error -> {:error, "the table wasn't successfully created"}
      :created -> :ok
    end
  end

  defp compile_analyst_table(query, table) do
    Cloak.AnalystTable.Compiler.compile(
      table.name,
      table.statement,
      table.analyst,
      query.data_source,
      query.parameters,
      query.views
    )
  end

  defp table_definition(table, table_query, alias) do
    Enum.zip(table_query.column_titles, table_query.columns)
    |> Enum.map(fn {title, column} -> %Expression{column | alias: title} end)
    |> Cloak.Sql.Compiler.Helpers.create_table_from_columns(alias)
    |> Map.put(:db_name, table.db_name)
  end

  defp report_error(table, reason),
    do: raise(Cloak.Sql.CompilationError, message: "Error in analyst table #{table.name}: #{reason}")
end
