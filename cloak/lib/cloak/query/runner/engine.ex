defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, DataSource, Query, ResultSender, Sql.Condition, Query.Probe}
  require Logger

  @type state_updater :: (ResultSender.query_state -> any)
  @type feature_updater :: (Sql.Compiler.Features.t -> any)


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
    |> build_initial_noise_layers()
    |> Probe.process()
    |> build_final_noise_layers()
    |> Sql.Compiler.LowCountChecks.compile()

  defp build_initial_noise_layers(query), do: Sql.Compiler.NoiseLayers.compile(query)

  defp build_final_noise_layers(query), do: Sql.Compiler.NoiseLayers.compile(query)

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query, features, _state_updater), do:
    Query.Result.new(
      query,
      features,
      Enum.map(
        (Map.keys(query.data_source.tables) ++ Map.keys(query.views)),
        &%{occurrences: 1, row: [to_string(&1)]}
      )
    )
  defp run_statement(%Sql.Query{command: :show, show: :columns} = query, features, _state_updater), do:
    Query.Result.new(
      query,
      features,
      Enum.map(
        sorted_table_columns(hd(query.selected_tables)),
        &%{occurrences: 1, row: [&1.name, to_string(&1.type)]}
      )
    )
  defp run_statement(%Sql.Query{command: :select} = query, features, state_updater), do:
    Cloak.Query.DataEngine.select(query, &process_final_rows(&1, query, features, state_updater))

  defp sorted_table_columns(table) do
    {[uid], other_columns} = Enum.split_with(table.columns, &(&1.name == table.user_id))
    [uid | other_columns]
  end

  defp process_final_rows(rows, query, features, state_updater) do
    Logger.debug("Processing final rows ...")

    {rows, query} = Query.RowSplitters.split(rows, query)

    rows
    |> Query.Rows.filter(Condition.to_function(query.emulated_where))
    |> Query.Aggregator.aggregate(query, features, state_updater)
  end
end
