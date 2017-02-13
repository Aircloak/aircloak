defmodule Cloak.Query.Runner.Engine do
  @moduledoc "Execution of SQL queries."
  alias Cloak.{Sql, DataSource, Query, Query.Error}
  require Logger


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Executes the SQL query and returns the query result or the corresponding error."
  @spec run(Sql.Query.t) :: {:ok, Query.Result.t} | %Error{}
  def run(query) do
    try do
      {:ok, run_statement(query)}
    rescue e in [Query.Runner.RuntimeError] ->
      %Error{
        type: :crash,
        context: "query failure",
        location: __MODULE__,
        human_description: e.message,
      }
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_statement(%Sql.Query{command: :show, show: :tables} = query), do:
    Query.Result.new(query,
      Enum.map(
        (Map.keys(query.data_source.tables) ++ Map.keys(query.views)),
        &%{occurrences: 1, row: [to_string(&1)]}
      )
    )
  defp run_statement(%Sql.Query{command: :show, show: :columns, selected_tables: [table]} = query), do:
    Query.Result.new(query,
      Enum.map(table.columns, fn({name, type}) -> %{occurrences: 1, row: [name, type]} end)
    )
  defp run_statement(%Sql.Query{command: :select} = query), do:
    select_rows(query)


  # -------------------------------------------------------------------
  # Handling of `SELECT` statement
  # -------------------------------------------------------------------

  defp select_rows(%Sql.Query{emulated?: false} = query) do
    DataSource.select!(query, fn(rows) ->
      rows
      |> Query.DataDecoder.decode(query)
      |> process_final_rows(%Sql.Query{query | where: query.emulated_where})
    end)
  end
  defp select_rows(%Sql.Query{emulated?: true} = query) do
    Logger.debug("Emulating query ...")
    query
    |> Query.DbEmulator.select()
    |> process_final_rows(query)
  end

  defp process_final_rows(rows, query) do
    Logger.debug("Processing final rows ...")
    rows
    |> Query.RowSplitters.split(query)
    |> Query.Rows.filter(Enum.map(query.where, &Sql.Comparison.to_function/1))
    |> Query.ShrinkAndDrop.apply(query)
    |> Query.Aggregator.aggregate(query)
    |> Query.Sorter.order_buckets(query)
    |> Query.Result.distinct(query.distinct?)
    |> Query.Result.offset(query.offset)
    |> Query.Result.limit(query.limit)
  end
end
