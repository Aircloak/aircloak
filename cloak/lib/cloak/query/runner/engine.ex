defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, DataSource, Query, ResultSender, Sql.Condition}
  alias Cloak.Query.Runner.ParallelProcessor
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
    |> Sql.Query.resolve_db_columns()
    |> Sql.Compiler.NoiseLayers.compile()

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query, features, _state_updater), do:
    (Map.keys(query.data_source.tables) ++ Map.keys(query.views))
    |> Enum.map(&%{occurrences: 1, row: [to_string(&1)]})
    |> Query.Result.new(query, features)
  defp run_statement(%Sql.Query{command: :show, show: :columns} = query, features, _state_updater), do:
    query.selected_tables
    |> hd()
    |> sorted_table_columns()
    |> Enum.map(&%{occurrences: 1, row: [&1.name, to_string(&1.type)]})
    |> Query.Result.new(query, features)
  defp run_statement(%Sql.Query{command: :select} = query, features, state_updater), do:
    select(query, &process_final_rows(&1, query, features, state_updater))

  defp select(%Sql.Query{emulated?: true} = query, rows_processor) do
    Logger.debug("Emulating query ...")
    query |> Query.DbEmulator.select() |> rows_processor.()
  end
  defp select(%Sql.Query{emulated?: false} = query, rows_processor) do
    DataSource.select!(%Sql.Query{query | where: Sql.Query.offloaded_where(query)}, rows_processor)
  end

  defp sorted_table_columns(table) do
    {[uid], other_columns} = Enum.split_with(table.columns, &(&1.name == table.user_id))
    [uid | other_columns]
  end

  defp process_final_rows(stream, query, features, state_updater) do
    Logger.debug("Processing final rows ...")

    query = Query.RowSplitters.compile(query)
    state_updater = fn (acc, state) -> state_updater.(state); acc end

    stream
    |> Stream.transform(:first, fn
      (chunk, :first) -> {[state_updater.(chunk, :ingesting_data)], :not_first}
      (chunk, :not_first) -> {[chunk], :not_first}
    end)
    |> ParallelProcessor.execute(concurrency(query),
      &consume_rows(&1, query), &Query.Aggregator.merge_groups/2)
    |> state_updater.(:processing)
    |> Query.Aggregator.aggregate(query)
    |> state_updater.(:post_processing)
    |> Query.Result.new(query, features)
  end

  defp consume_rows(stream, query) do
    stream
    |> decode_rows(query)
    |> Query.RowSplitters.split(query)
    |> Query.Rows.filter(query |> Sql.Query.emulated_where() |> Condition.to_function())
    |> Query.Aggregator.group(query)
  end

  defp decode_rows(stream, %Sql.Query{emulated?: true}), do: stream
  defp decode_rows(stream, query), do: Query.DataDecoder.decode(stream, query)

  defp concurrency(query), do:
    query.data_source.concurrency || Application.get_env(:cloak, :concurrency, 0)
end
