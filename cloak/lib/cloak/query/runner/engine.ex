defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, DataSource, Query, ResultSender}
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
        state_updater.(:awaiting_data)
      do
        query_killer_reg.()
        result = run_statement(query, state_updater)
        query_killer_unreg.()
        {:ok, result, Sql.Query.info_messages(query)}
      end
    rescue e in [Query.Runner.RuntimeError, RuntimeError] ->
      {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query, _state_updater), do:
    Query.Result.new(query,
      Enum.map(
        (Map.keys(query.data_source.tables) ++ Map.keys(query.views)),
        &%{occurrences: 1, row: [to_string(&1)]}
      )
    )
  defp run_statement(%Sql.Query{command: :show, show: :columns, selected_tables: [table]} = query, _state_updater), do:
    Query.Result.new(query,
      Enum.map(table.columns, fn({name, type}) -> %{occurrences: 1, row: [name, type]} end)
    )
  defp run_statement(%Sql.Query{command: :select} = query, state_updater), do:
    select_rows(query, state_updater)


  # -------------------------------------------------------------------
  # Handling of `SELECT` statement
  # -------------------------------------------------------------------

  defp select_rows(%Sql.Query{emulated?: false} = query, state_updater) do
    DataSource.select!(query, fn(rows) ->
      rows
      |> Query.DataDecoder.decode(query)
      |> process_final_rows(%Sql.Query{query | where: query.emulated_where}, state_updater)
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
    |> Cloak.Stream.side_effect_after_first(fn() -> state_updater.(:ingesting_data) end)
    |> Cloak.Stream.side_effect_after_last(fn() -> state_updater.(:processing) end)
    |> Query.RowSplitters.split(query)
    |> Query.Rows.filter(Enum.map(query.where, &Sql.Comparison.to_function/1))
    |> Query.ShrinkAndDrop.apply(query)
    |> Query.Aggregator.aggregate(query)
    |> fn(rows) -> state_updater.(:post_processing); rows end.()
    |> Query.Sorter.order_buckets(query)
    |> Query.Result.distinct(query.distinct?)
    |> Query.Result.offset(query.offset)
    |> Query.Result.limit(query.limit)
  end
end
