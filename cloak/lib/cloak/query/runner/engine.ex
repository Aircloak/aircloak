defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, DataSource, Query, ResultSender, Sql.Condition, Query.DataEngine}
  require Logger

  @type state_updater :: (ResultSender.query_state -> any)
  @type feature_updater :: (Query.features -> any)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the SQL query and returns the query result with info messages or the corresponding error."
  @spec run(DataSource.t, String.t, [DataSource.field], Sql.Query.view_map, state_updater, feature_updater,
    Cloak.MemoryReader.query_killer_callbacks) :: {:ok, Sql.Query.Result.t, [String.t]} | {:error, String.t}
  def run(data_source, statement, parameters, views, state_updater, feature_updater,
      {query_killer_reg, query_killer_unreg}) do
    try do
      with \
        {:ok, parsed_query} <- parse(statement, state_updater),
        {:ok, compiled_query, features} <- compile(data_source, parsed_query, parameters, views, state_updater)
      do
        feature_updater.(features)
        query = prepare_for_execution(compiled_query)
        state_updater.(:awaiting_data)
        query_killer_reg.()
        result = run_statement(query, features, state_updater)
        query_killer_unreg.()
        {:ok, result, Sql.Query.info_messages(query)}
      end
    rescue e in Cloak.Query.ExecutionError ->
      {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp parse(statement, state_updater) do
    state_updater.(:parsing)
    Sql.Parser.parse(statement)
  end

  defp compile(data_source, parsed_query, parameters, views, state_updater) do
    state_updater.(:compiling)
    Sql.Compiler.compile(data_source, parsed_query, parameters, views)
  end

  defp prepare_for_execution(compiled_query), do:
    compiled_query
    |> Cloak.Sql.Query.resolve_db_columns()
    |> Sql.Compiler.NoiseLayers.compile()

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query, features, _state_updater) do
    buckets =
      (Map.keys(query.data_source.tables) ++ Map.keys(query.views))
      |> Enum.map(&%{occurrences: 1, row: [to_string(&1)]})
    Query.Result.new({buckets, 0}, query, features)
  end
  defp run_statement(%Sql.Query{command: :show, show: :columns} = query, features, _state_updater) do
    buckets =
      query.selected_tables
      |> hd()
      |> sorted_table_columns()
      |> Enum.map(&%{occurrences: 1, row: [&1.name, to_string(&1.type)]})
    Query.Result.new({buckets, 0}, query, features)
  end
  defp run_statement(%Sql.Query{command: :select} = query, features, state_updater), do:
    Cloak.Query.DataEngine.select(query, &process_final_rows(&1, query, features, state_updater))

  defp sorted_table_columns(table) do
    {[uid], other_columns} = Enum.split_with(table.columns, &(&1.name == table.user_id))
    [uid | other_columns]
  end

  defp process_final_rows(rows, query, features, state_updater) do
    Logger.debug("Processing final rows ...")

    query = Query.RowSplitters.compile(query)
    state_updater = fn (acc, state) -> state_updater.(state); acc end

    rows
    |> Query.RowSplitters.split(query)
    |> Query.Rows.filter(query |> DataEngine.emulated_where() |> Condition.to_function())
    |> state_updater.(:ingesting_data)
    |> Query.Aggregator.group(query)
    |> state_updater.(:processing)
    |> Query.Aggregator.aggregate(query)
    |> state_updater.(:post_processing)
    |> Query.Result.new(query, features)
  end
end
