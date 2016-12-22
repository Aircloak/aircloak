defmodule Cloak.Query.DbEmulator do
  @moduledoc """
  Database emulator for executing non-anonymized queries in the cloak.

  There are some cases in which we wish to execute a non-anonymized query inside the cloak,
  as opposed to sending it to the database server (for example, if some columns require
  decoding or if a JOIN requires data from two different data sources).
  """
  require Logger

  alias Cloak.{Aql.Query, DataSource, Query.DbEmulator.Selector}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Retrieves rows according to the specification in the emulated query."
  @spec select(Query.t) :: Enumerable.t
  def select(%Query{emulated?: true} = query), do:
    query.from
    |> select_rows()
    |> Selector.pick_db_columns(query)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp select_rows({:subquery, %{ast: %Query{emulated?: true, from: from} = subquery}}) when not is_binary(from) do
    Logger.debug("Emulating sub-query ...")
    rows = select_rows(from)
    Logger.debug("Processing rows ...")
    rows
    |> Selector.pick_db_columns(subquery)
    |> Selector.select(subquery)
    |> Enum.to_list()
  end
  defp select_rows({:subquery, %{ast: %Query{} = query}}) do
    # either a non-emulated subquery, or a subquery selecting from a single table
    Logger.debug("Loading sub-query through data source ...")
    DataSource.select!(%Query{query | subquery?: false}, fn(rows) ->
      Logger.debug("Processing rows ...")
      rows
      |> Cloak.Query.DataDecoder.decode(query)
      |> Selector.select(%Query{query | where: query.encoded_where, encoded_where: []})
      |> Enum.to_list()
    end)
  end
  defp select_rows({:join, join}) do
    Logger.debug("Emulating join ...")
    rhs_task = Task.async(fn() -> select_rows(join.rhs) end)
    lhs_rows = select_rows(join.lhs)
    rhs_rows = Task.await(rhs_task, :infinity)
    Selector.join(lhs_rows, rhs_rows, join)
  end
end
