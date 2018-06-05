defmodule Cloak.DataSource.Isolators.Cache do
  @moduledoc """
  Implementation of the cache which holds the isolator property of all known columns of all data sources.
  """

  @refresh_interval :timer.hours(24)

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource.Isolators.{Queue, CacheOwner}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns true if the given column in the given table is isolating, false otherwise."
  @spec isolates_users?(atom | pid, Cloak.DataSource.t(), String.t(), String.t()) :: boolean
  def isolates_users?(cache_ref \\ __MODULE__, data_source, table_name, column_name) do
    column = {data_source.name, table_name, column_name}

    with :error <- CacheOwner.lookup(column),
         :error <- GenServer.call(cache_ref, {:fetch_isolation, column}, :infinity) do
      raise RuntimeError, "Cannot determine isolated property of #{table_name}.#{column_name}"
    else
      {:ok, isolates?} -> isolates?
    end
  end

  @doc "Invoked when data sources have been changed."
  @spec data_sources_changed(atom | pid) :: :ok
  def data_sources_changed(cache_ref \\ __MODULE__), do: GenServer.cast(cache_ref, :data_sources_changed)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(opts) do
    enqueue_next_refresh()
    known_columns = MapSet.new(opts.columns_provider.())
    state = %{known_columns: known_columns, queue: Queue.new(known_columns), waiting: %{}, opts: opts}
    {:ok, start_next_computation(state)}
  end

  @impl GenServer
  def handle_call({:fetch_isolation, column}, from, state) do
    cond do
      not MapSet.member?(state.known_columns, column) ->
        {:reply, :error, state}

      # doing another lookup, because property might have become available while this request was in the queue
      match?({:ok, _}, CacheOwner.lookup(column)) ->
        {:ok, isolates?} = CacheOwner.lookup(column)
        {:reply, {:ok, isolates?}, state}

      computing_isolation?(column) or not Queue.processed?(state.queue, column) ->
        {:noreply, add_waiting_request(state, column, from)}

      true ->
        {:reply, :error, state}
    end
  end

  @impl GenServer
  def handle_cast(:data_sources_changed, state) do
    known_columns = MapSet.new(state.opts.columns_provider.())
    state = %{state | known_columns: known_columns}
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
    result = CacheOwner.lookup(meta.column)
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
          start: fn -> start_compute_isolation(column, state.opts.compute_isolation_fun) end,
          meta: %{column: column}
        })

        %{state | queue: queue}

      :error ->
        state
    end
  end

  defp start_compute_isolation(column, compute_isolation_fun) do
    Task.start_link(fn ->
      Logger.debug(fn -> "computing isolated for #{inspect(column)}" end)
      isolated = compute_isolation_fun.(column)
      CacheOwner.store(column, isolated)
    end)
  end

  defp compute_column_isolation({data_source_name, table_name, column_name}) do
    {:ok, data_source} = Cloak.DataSource.fetch(data_source_name)
    Cloak.DataSource.Isolators.Query.isolates_users?(data_source, table_name, column_name)
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

  defp add_waiting_request(state, column, from) do
    queue = if computing_isolation?(column), do: state.queue, else: Queue.set_high_priority(state.queue, column)
    waiting = Map.update(state.waiting, column, [from], &[from | &1])
    %{state | waiting: waiting, queue: queue}
  end

  defp computing_isolation?(column),
    do: match?({:ok, %{column: ^column}}, Parent.GenServer.child_meta(:compute_isolation_job))

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
  def start_link(opts \\ []) do
    opts =
      [columns_provider: &known_columns/0, compute_isolation_fun: &compute_column_isolation/1, registered?: true]
      |> Keyword.merge(opts)
      |> Map.new()

    gen_server_opts = if opts.registered?, do: [name: __MODULE__], else: []
    Parent.GenServer.start_link(__MODULE__, opts, gen_server_opts)
  end
end
