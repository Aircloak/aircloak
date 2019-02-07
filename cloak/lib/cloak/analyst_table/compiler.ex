defmodule Cloak.AnalystTable.Compiler do
  @moduledoc "Compilation of analyst table query."

  alias Cloak.Sql.Query

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
         :ok <- verify_query_type(query),
         :ok <- verify_offloading(query),
         :ok <- verify_selected_columns(query),
         do: {:ok, query}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp compile_statement(statement, analyst, data_source, parameters, views) do
    with {:ok, parsed_query} <- Cloak.Sql.Parser.parse(statement),
         {:ok, query} <- Cloak.Sql.Compiler.compile_direct(parsed_query, analyst, data_source, parameters, views),
         do: {:ok, query |> Query.set_emulation_flag() |> Cloak.Sql.Compiler.Anonymization.set_query_type()}
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
    duplicates =
      query.column_titles
      |> Stream.transform(
        {MapSet.new(), MapSet.new()},
        fn title, {duplicates, uniques} ->
          cond do
            MapSet.member?(duplicates, title) -> {[], {duplicates, uniques}}
            MapSet.member?(uniques, title) -> {[title], {MapSet.put(duplicates, title), MapSet.delete(uniques, title)}}
            true -> {[], {duplicates, MapSet.put(uniques, title)}}
          end
        end
      )
      |> Enum.to_list()

    if Enum.empty?(duplicates),
      do: :ok,
      else: {:error, "Duplicate column names: #{duplicates |> Stream.map(&"`#{&1}`") |> Enum.join(", ")}"}
  end
end
