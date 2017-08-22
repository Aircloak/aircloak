defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, DataSource, Query, ResultSender, Sql.Condition, Query.Probe}
  require Logger

  @type state_updater :: (ResultSender.query_state -> any)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the SQL query and returns the query result with info messages or the corresponding error."
  @spec run(DataSource.t, String.t, [DataSource.field], Sql.Query.view_map, state_updater,
    Cloak.MemoryReader.query_killer_callbacks) :: {:ok, Sql.Query.Result.t, [String.t]} | {:error, String.t}
  def run(data_source, statement, parameters, views, state_updater,
      {query_killer_reg, query_killer_unreg}) do
    try do
      with state_updater.(:parsing),
        {:ok, parsed} <- Sql.Parser.parse(statement),
        state_updater.(:compiling),
        {:ok, query} <- Sql.Compiler.compile(data_source, parsed, parameters, views),
        query = build_initial_noise_layers(query),
        query = Probe.process(query),
        query = build_final_noise_layers(query),
        state_updater.(:awaiting_data)
      do
        query_killer_reg.()
        result = run_statement(query, state_updater)
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

  defp build_initial_noise_layers(query), do: Sql.Compiler.NoiseLayers.compile(query)

  defp build_final_noise_layers(query), do: Sql.Compiler.NoiseLayers.compile(query)

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query, _state_updater), do:
    Query.Result.new(query,
      Enum.map(
        (Map.keys(query.data_source.tables) ++ Map.keys(query.views)),
        &%{occurrences: 1, row: [to_string(&1)]}
      )
    )
  defp run_statement(%Sql.Query{command: :show, show: :columns, selected_tables: [table]} = query, _state_updater), do:
    Query.Result.new(
      query,
      Enum.map(sorted_table_columns(table),
      &%{occurrences: 1, row: [&1.name, to_string(&1.type)]})
    )
  defp run_statement(%Sql.Query{command: :select} = query, state_updater), do:
    select_rows(query, state_updater)

  defp sorted_table_columns(table) do
    {[uid], other_columns} = Enum.split_with(table.columns, &(&1.name == table.user_id))
    [uid | other_columns]
  end


  # -------------------------------------------------------------------
  # Handling of `SELECT` statement
  # -------------------------------------------------------------------

  defp select_rows(%Sql.Query{emulated?: false} = query, state_updater) do
    DataSource.select!(query, fn(rows) ->
      rows
      |> Query.DataDecoder.decode(query)
      |> process_final_rows(query, state_updater)
    end)
  end
  defp select_rows(%Sql.Query{emulated?: true} = query, state_updater) do
    Logger.debug("Emulating query ...")
    query
    |> Query.DbEmulator.select()
    |> process_final_rows(query, state_updater)
  end

  defp process_final_rows(rows, query, state_updater) do
    Logger.debug("Processing final rows ...")
    rows
    |> Query.RowSplitters.split(query)
    |> Query.Rows.filter(Condition.to_function(query.emulated_where))
    |> Query.Aggregator.aggregate(query, state_updater)
  end
end
