defmodule Cloak.Query.Select do
  @moduledoc "Handles the processing of the `SELECT` statement."
  alias Cloak.{Aql, DataSource, Query}
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the SELECT query and returns the query result or the corresponding error"
  @spec run(Aql.Query.t) :: {:ok, Query.Result.t} | {:error, String.t}
  def run(%Aql.Query{command: :select} = query) do
    try do
      select_rows(query)
    rescue e in [RuntimeError] ->
      {:error, e.message}
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp select_rows(%Aql.Query{subquery?: false, emulated?: false} = query) do
    DataSource.select(query, fn(rows) ->
      process_final_rows(rows, %Aql.Query{query | where: query.encoded_where})
    end)
  end
  defp select_rows(%Aql.Query{subquery?: false, emulated?: true} = query) do
    Logger.debug("Emulating query ...")
    with {:ok, rows} <- select_rows(query.from) do
      {:ok, process_final_rows(rows, query)}
    end
  end
  defp select_rows({:subquery, %{ast: %Aql.Query{emulated?: true, from: from} = subquery}}) when not is_binary(from) do
    Logger.debug("Emulating query ...")
    with {:ok, rows} <- select_rows(from) do
      Logger.debug("Processing rows ...")
      rows =
        rows
        |> Query.DBEmulator.select(subquery)
        |> Enum.to_list()
      {:ok, rows}
    end
  end
  defp select_rows({:subquery, %{ast: subquery}}) do
    select_rows(subquery)
  end
  defp select_rows({:join, join}) do
    Logger.debug("Emulating join ...")
    {:ok, lhs} = select_rows(join.lhs)
    {:ok, rhs} = select_rows(join.rhs)
    {:ok, Query.DBEmulator.join(lhs, rhs, join) |> Enum.to_list()}
  end
  defp select_rows(%Aql.Query{} = query) do
    Logger.debug("Emulating query ...")
    DataSource.select(%Aql.Query{query | subquery?: false}, fn(rows) ->
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
