defmodule Cloak.Sql.Compiler.AnalystTables do
  @moduledoc "Compliaction of analyst tables."

  alias Cloak.Sql.{Expression, Query}
  alias Cloak.Sql.Query.Lenses
  alias Cloak.Sql.Compiler.Helpers

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Replaces subqueries which refer to analyst tables with references to tables on the disk."
  @spec compile(Query.t()) :: Query.t()
  def compile(query) do
    Helpers.apply_top_down(
      query,
      fn query -> if uses_analyst_tables?(query), do: replace_analyst_tables(query), else: query end
    )
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp uses_analyst_tables?(query) do
    query
    |> get_in([Lenses.subqueries()])
    |> Enum.any?(&(not is_nil(&1.analyst_table)))
  end

  defp replace_analyst_tables(query) do
    tables =
      query
      |> get_in([Lenses.analyst_tables_subqueries()])
      |> Enum.map(&analyst_table_definition/1)

    query = %Query{query | available_tables: query.available_tables ++ tables}

    selected_tables = Enum.reject(query.selected_tables, fn table -> Enum.any?(tables, &(&1.name == table.name)) end)
    query = %Query{query | selected_tables: selected_tables ++ tables}

    Lens.map(
      Lenses.analyst_tables_subqueries(),
      query,
      fn {:subquery, %{ast: %{analyst_table: table}}} -> table.name end
    )
  end

  defp analyst_table_definition({:subquery, %{ast: %{analyst_table: table} = query}}) do
    case Cloak.AnalystTable.find(query.analyst_id, table.name, query.data_source).status do
      :creating ->
        raise Cloak.Sql.CompilationError, message: "The table `#{table.name}` is still being created."

      :create_error ->
        raise Cloak.Sql.CompilationError, message: "An error happened while creating the table `#{table.name}`."

      :created ->
        :ok
    end

    case Cloak.AnalystTable.Compiler.compile(
           table.name,
           table.statement,
           table.analyst,
           query.data_source,
           query.parameters,
           query.views
         ) do
      {:ok, table_query} ->
        Enum.zip(table_query.column_titles, table_query.columns)
        |> Enum.map(fn {title, column} -> %Expression{column | alias: title} end)
        |> Cloak.Sql.Compiler.Helpers.create_table_from_columns(table.name)
        |> Map.put(:db_name, table.db_name)

      {:error, reason} ->
        raise Cloak.Sql.CompilationError, message: "Error in analyst table #{table.name}: #{reason}"
    end
  end
end
