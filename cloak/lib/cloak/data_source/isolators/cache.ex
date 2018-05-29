defmodule Cloak.DataSource.Isolators.Cache do
  @moduledoc """
  Implementation of the cache which holds the isolator property of all known columns of all data sources.
  """

  use Parent.GenServer
  alias Cloak.DataSource.Isolators.Queue

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(String.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table, column),
    do: Cloak.DataSource.Isolators.Query.isolates_users?(data_source, table, column)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    {:ok, Queue.new(columns(Cloak.DataSource.all()))}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp columns(data_sources), do: Enum.flat_map(data_sources, &data_source_columns/1)
  defp data_source_columns(data_source), do: Enum.flat_map(data_source.tables, &table_columns(data_source, &1))

  defp table_columns(data_source, {_table_id, table}) do
    table.columns
    |> Enum.reject(&(&1.name == table.user_id))
    |> Enum.map(&{data_source.name, table.name, &1.name})
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
