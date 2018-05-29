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
  @spec isolates_users?(Cloak.DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(data_source, table_name, column_name) do
    column = {data_source.name, table_name, column_name}

    case lookup_cache(column) do
      {:ok, isolates?} ->
        isolates?

      :error ->
        {:ok, isolates?} = GenServer.call(__MODULE__, {:fetch_isolation, column}, :infinity)
        isolates?
    end
  end

  @doc "Invoked when data sources have been changed."
  @spec data_sources_changed() :: :ok
  def data_sources_changed(), do: GenServer.cast(__MODULE__, :data_sources_changed)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true])
    state = %{queue: Queue.new(known_columns()), waiting: %{}}
    {:ok, start_next_computation(state)}
  end

  @impl GenServer
  def handle_call({:fetch_isolation, column}, from, state) do
    # doing another lookup, because property might have become available while this request was in the queue
    case lookup_cache(column) do
      {:ok, isolates?} -> {:reply, {:ok, isolates?}, state}
      :error -> {:noreply, add_waiting_request(state, column, from)}
    end
  end

  @impl GenServer
  def handle_cast(:data_sources_changed, state) do
    state = update_in(state.queue, &Queue.update_known_columns(&1, known_columns()))
    state = if Parent.GenServer.child?(:compute_isolation_job), do: state, else: start_next_computation(state)
    {:noreply, state}
  end

  @impl Parent.GenServer
  def handle_child_terminated(:compute_isolation_job, meta, _pid, _reason, state) do
    result = lookup_cache(meta.column)
    state.waiting |> Map.get(meta.column, []) |> Enum.each(&GenServer.reply(&1, result))
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
          start: {Task, :start_link, [fn -> compute_column_isolation(column) end]},
          meta: %{column: column}
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

  defp known_columns(), do: Enum.flat_map(Cloak.DataSource.all(), &data_source_columns/1)

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

  defp lookup_cache(column) do
    case :ets.match(__MODULE__, {column, :"$1"}) do
      [[isolates?]] -> {:ok, isolates?}
      [] -> :error
    end
  end

  defp add_waiting_request(state, column, from) do
    state = %{state | waiting: Map.update(state.waiting, column, [from], &[from | &1])}

    case state.waiting[column] do
      # first waiting client -> raise prio
      [_] ->
        update_in(state.queue, &Queue.set_high_priority(&1, column))

      # multiple waiting clients -> prio was already raised, so no need to do anything else
      [_, _ | _] ->
        state
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
