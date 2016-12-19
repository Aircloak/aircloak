defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of AQL queries."
  alias Cloak.{Aql, DataSource, Query}
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the AQL query and returns the query result or the corresponding error."
  @spec run(Aql.Query.t) :: {:ok, Query.Result.t} | {:error, String.t}
  def run(query) do
    try do
      {:ok, run_statement(query)}
    rescue e in [Query.Runner.RuntimeError] ->
      {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_statement(%Aql.Query{command: :show, show: :tables} = query), do:
    Query.Result.new(query,
      Enum.map(
        (Map.keys(query.data_source.tables) ++ Map.keys(query.views)),
        &%{occurrences: 1, row: [to_string(&1)]}
      )
    )
  defp run_statement(%Aql.Query{command: :show, show: :columns, selected_tables: [table]} = query), do:
    Query.Result.new(query,
      Enum.map(table.columns, fn({name, type}) -> %{occurrences: 1, row: [name, type]} end)
    )
  defp run_statement(%Aql.Query{command: :select} = query), do:
    select_rows(query)


  # -------------------------------------------------------------------
  # Handling of `SELECT` statement
  # -------------------------------------------------------------------

  defp select_rows(%Aql.Query{subquery?: false, emulated?: false} = query) do
    DataSource.select!(query, fn(rows) ->
      process_final_rows(rows, %Aql.Query{query | where: query.encoded_where})
    end)
  end
  defp select_rows(%Aql.Query{subquery?: false, emulated?: true} = query) do
    Logger.debug("Emulating top query ...")
    query.from
    |> select_rows()
    |> process_final_rows(query)
  end
  defp select_rows({:subquery, %{ast: %Aql.Query{emulated?: true, from: from} = subquery}}) when not is_binary(from) do
    Logger.debug("Emulating sub-query ...")
    rows = select_rows(from)

    Logger.debug("Processing rows ...")
    rows
    |> Query.DBEmulator.select(subquery)
    |> Enum.to_list()
  end
  defp select_rows({:subquery, %{ast: subquery}}), do:
    select_rows(subquery)
  defp select_rows({:join, join}) do
    Logger.debug("Emulating join ...")
    Query.DBEmulator.join(select_rows(join.lhs), select_rows(join.rhs), join)
    |> Enum.to_list()
  end
  defp select_rows(%Aql.Query{} = query) do
    Logger.debug("Emulating sub-query ...")
    DataSource.select!(%Aql.Query{query | subquery?: false}, fn(rows) ->
      Logger.debug("Processing rows ...")
      rows
      |> Query.DataDecoder.decode(query)
      |> Query.DBEmulator.select(%Aql.Query{query | where: query.encoded_where, encoded_where: []})
      |> Enum.to_list()
    end)
  end

  defp process_final_rows(rows, query) do
    Logger.debug("Processing final rows ...")
    rows
    |> Query.DataDecoder.decode(query)
    |> Query.RowSplitters.split(query)
    |> Query.DBEmulator.filter_rows(query)
    |> Query.LCFConditions.apply(query)
    |> Query.ShrinkAndDrop.apply(query)
    |> Query.Aggregator.aggregate(query)
    |> Query.Sorter.order_buckets(query)
    |> Query.Result.distinct(query.distinct?)
    |> Query.Result.offset(query.offset)
    |> Query.Result.limit(query.limit)
  end
end
