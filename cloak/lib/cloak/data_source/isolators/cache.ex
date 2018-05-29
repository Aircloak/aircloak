defmodule Cloak.DataSource.Isolators.Cache do
  @moduledoc """
  Implementation of the cache which holds the isolator property of all known columns of all data sources.
  """

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource.Isolators.Queue

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(String.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table, column) do
    case :ets.match(__MODULE__, {{data_source.name, table, column}, :"$1"}) do
      [[isolates?]] -> isolates?
    end
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true])
    state = %{queue: Queue.new(columns(Cloak.DataSource.all()))}
    {:ok, start_next_computation(state)}
  end

  @impl Parent.GenServer
  def handle_child_terminated(:compute_isolation_job, _meta, _pid, _reason, state) do
    {:noreply, start_next_computation(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp start_next_computation(state) do
    case Queue.next_column(state.queue) do
      {column, queue} ->
        Parent.GenServer.start_child(%{
          id: :compute_isolation_job,
          start: {Task, :start_link, [fn -> compute_column_isolation(column) end]}
        })

        %{state | queue: queue}

      :error ->
        state
    end
  end

  defp compute_column_isolation({data_source_name, table_name, column_name} = column) do
    Logger.debug(fn -> "computing isolated for #{inspect(column)}" end)
    {:ok, data_source} = Cloak.DataSource.fetch(data_source_name)
    isolation = Cloak.DataSource.Isolators.Query.isolates_users?(data_source, table_name, column_name)
    :ets.insert(__MODULE__, {column, isolation})
  end

  defp columns(data_sources), do: Enum.flat_map(data_sources, &data_source_columns/1)

  defp data_source_columns(data_source) do
    data_source.tables
    # credo:disable-for-next-line Credo.Check.Design.TagTODO
    # TODO skipping virtual tables for now, because isolator query doesn't work with them
    |> Enum.reject(fn {_table_id, table} -> not is_nil(table.query) end)
    |> Enum.flat_map(&table_columns(data_source, &1))
  end

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
