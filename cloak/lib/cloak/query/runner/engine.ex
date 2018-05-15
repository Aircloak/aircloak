defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, DataSource, Query, ResultSender}

  require Logger

  @type state_updater :: (ResultSender.query_state() -> any)
  @type feature_updater :: (Query.features() -> any)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the SQL query and returns the query result with info messages or the corresponding error."
  @spec run(
          DataSource.t(),
          String.t(),
          [DataSource.field()],
          Sql.Query.view_map(),
          state_updater,
          feature_updater,
          Cloak.MemoryReader.query_killer_callbacks()
        ) :: {:ok, Sql.Query.Result.t(), [String.t()]} | {:error, String.t()}
  def run(
        data_source,
        statement,
        parameters,
        views,
        state_updater,
        feature_updater,
        {query_killer_reg, query_killer_unreg}
      ) do
    query =
      statement
      |> parse!(state_updater)
      |> compile!(data_source, parameters, views, state_updater)

    features = Sql.Query.features(query)

    feature_updater.(features)
    query_killer_reg.()
    result = run_statement(query, features, state_updater)
    query_killer_unreg.()
    {:ok, result, Sql.Query.info_messages(query)}
  rescue
    e in Cloak.Query.ExecutionError ->
      {:error, e.message}

    e in [Cloak.Sql.CompilationError, Cloak.Sql.ParseError] ->
      {:error, Cloak.Sql.ErrorFormat.format(statement, e)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parse!(statement, state_updater) do
    state_updater.(:parsing)
    Sql.Parser.parse!(statement)
  end

  defp compile!(parsed_query, data_source, parameters, views, state_updater) do
    state_updater.(:compiling)
    Sql.Compiler.compile!(parsed_query, data_source, parameters, views)
  end

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query, features, _state_updater),
    do:
      (Map.keys(query.data_source.tables) ++ Map.keys(query.views))
      |> Enum.map(&%{occurrences: 1, row: [to_string(&1)]})
      |> Query.Result.new(query.column_titles, features)

  defp run_statement(
         %Sql.Query{command: :show, show: :columns} = query,
         features,
         _state_updater
       ),
       do:
         query.selected_tables
         |> hd()
         |> sorted_table_columns()
         |> Enum.map(&%{occurrences: 1, row: [&1.name, to_string(&1.type)]})
         |> Query.Result.new(query.column_titles, features)

  defp run_statement(%Sql.Query{command: :select} = query, features, state_updater),
    do:
      query
      |> Query.DbEmulator.select(state_updater)
      |> Query.Result.new(query.column_titles, features)

  defp sorted_table_columns(table) do
    {[uid], other_columns} = Enum.split_with(table.columns, &(&1.name == table.user_id))
    [uid | other_columns]
  end
end
