defmodule Cloak.DataSource.Isolators.Cache do
  @moduledoc """
  Implementation of the cache which holds the isolator property of all known columns of all data sources.
  """

  @refresh_interval :timer.hours(24)

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
    enqueue_next_refresh()
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
    known_columns = known_columns()
    state = update_in(state.queue, &Queue.update_known_columns(&1, known_columns))
    state = respond_error_on_missing_columns(state, known_columns)
    {:noreply, maybe_start_next_computation(state)}
  end

  @impl GenServer
  def handle_info(:refresh, state) do
    # Refresh is handled by resetting the queue (see `Queue.reset/1` for details), which means that the previously
    # processed columns are moved to the back of the queue.
    # This gives us a simple solution to the overload problem. If we can't compute all columns during the refresh
    # interval, we'll end up constantly refreshing, but tail columns won't starve, and no queue will grow indefinitely.
    state = maybe_start_next_computation(update_in(state.queue, &Queue.reset/1))
    enqueue_next_refresh()
    {:noreply, state}
  end

  def handle_info(other, state), do: super(other, state)

  @impl Parent.GenServer
  def handle_child_terminated(:compute_isolation_job, meta, _pid, _reason, state) do
    result = lookup_cache(meta.column)
    state.waiting |> Map.get(meta.column, []) |> Enum.each(&GenServer.reply(&1, result))
    {:noreply, start_next_computation(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp maybe_start_next_computation(state) do
    if Parent.GenServer.child?(:compute_isolation_job), do: state, else: start_next_computation(state)
  end

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

  defp table_columns(data_source, {_table_id, table}),
    do: Enum.map(table.columns, &{data_source.name, table.name, &1.name})

  defp lookup_cache(column) do
    if column in known_columns() do
      do_lookup_cache(column)
    else
      raise "Unknown column #{inspect(column)}."
    end
  end

  defp do_lookup_cache(column) do
    case :ets.match(__MODULE__, {column, :"$1"}) do
      [[isolates?]] -> {:ok, isolates?}
      [] -> :error
    end
  end

  defp add_waiting_request(state, column, from) do
    %{
      state
      | waiting: Map.update(state.waiting, column, [from], &[from | &1]),
        queue: Queue.set_high_priority(state.queue, column)
    }
  end

  defp respond_error_on_missing_columns(state, known_columns) do
    known_columns = MapSet.new(known_columns)
    {good, missing} = Enum.split_with(state.waiting, fn {column, _clients} -> MapSet.member?(known_columns, column) end)
    Enum.each(missing, fn {_column, clients} -> Enum.each(clients, &GenServer.reply(&1, :error)) end)
    %{state | waiting: Map.new(good)}
  end

  defp enqueue_next_refresh(), do: Process.send_after(self(), :refresh, @refresh_interval)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
