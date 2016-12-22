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
  @spec select_rows(Query.t) :: Enumerable.t
  def select_rows(%Query{emulated?: true} = query), do:
    query.from
    |> select_emulated_rows()
    |> Selector.pick_db_columns(query)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp select_emulated_rows({:subquery, %{ast: %Query{emulated?: true, from: from} = subquery}}) when not is_binary(from) do
    Logger.debug("Emulating sub-query ...")
    rows = select_emulated_rows(from)
    Logger.debug("Processing rows ...")
    rows
    |> Selector.pick_db_columns(subquery)
    |> Selector.select(subquery)
    |> Enum.to_list()
  end
  defp select_emulated_rows({:subquery, %{ast: %Query{} = query}}) do
    Logger.debug("Loading sub-query through data source ...")
    DataSource.select!(%Query{query | subquery?: false}, fn(rows) ->
      Logger.debug("Processing rows ...")
      rows
      |> Cloak.Query.DataDecoder.decode(query)
      |> Selector.select(%Query{query | where: query.encoded_where, encoded_where: []})
      |> Enum.to_list()
    end)
  end
  defp select_emulated_rows({:join, join}) do
    Logger.debug("Emulating join ...")
    Selector.join(select_emulated_rows(join.lhs), select_emulated_rows(join.rhs), join)
    |> Enum.to_list()
  end
end
