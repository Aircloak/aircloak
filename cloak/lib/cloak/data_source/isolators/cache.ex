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
         {:error, :failed} <- GenServer.call(cache_ref, {:fetch_isolation, column}, :infinity) do
      Logger.error("Cannot determine isolated property of `#{table_name}`.`#{column_name}`")
      true
    else
      {:ok, isolates?} -> isolates?
      {:error, :unknown_column} -> raise "Unknown column `#{table_name}`.`#{column_name}`"
    end
  end

  @doc "Performs a cache lookup."
  @spec lookup(Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, boolean} | {:error, :pending | :failed | :unknown_column}
  def lookup(cache_ref \\ __MODULE__, data_source, table_name, column_name) do
    GenServer.call(cache_ref, {:column_status, {data_source.name, table_name, column_name}})
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(opts) do
    if opts.auto_refresh?, do: Cloak.DataSource.subscribe_to_changes()
    enqueue_next_refresh()
    known_columns = MapSet.new(opts.columns_provider.(Cloak.DataSource.all()))
    queue = Queue.new(known_columns, CacheOwner.cached_columns())
    state = %{known_columns: known_columns, queue: queue, waiting: %{}, opts: opts}
    {:ok, start_next_computation(state)}
  end

  @impl GenServer
  def handle_call({:fetch_isolation, column}, from, state) do
    case column_status(column, state) do
      {:ok, result} -> {:reply, {:ok, result}, state}
      {:error, :pending} -> {:noreply, add_waiting_request(state, column, from)}
      {:error, other} -> {:reply, {:error, other}, state}
    end
  end

  def handle_call({:column_status, column}, _from, state) do
    {:reply, column_status(column, state), state}
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

  def handle_info({:data_sources_changed, new_data_sources}, state) do
    known_columns = MapSet.new(state.opts.columns_provider.(new_data_sources))
    CacheOwner.remove_unknown_columns(known_columns)
    state = %{state | known_columns: known_columns}
    state = update_in(state.queue, &Queue.update_known_columns(&1, known_columns))
    state = respond_error_on_missing_columns(state)
    {:noreply, maybe_start_next_computation(state)}
  end

  @impl Parent.GenServer
  def handle_child_terminated(:compute_isolation_job, meta, _pid, _reason, state) do
    result =
      case CacheOwner.lookup(meta.column) do
        :error -> {:error, :failed}
        {:ok, result} -> {:ok, result}
      end

    state.waiting |> Map.get(meta.column, []) |> Enum.each(&GenServer.reply(&1, result))
    {:noreply, start_next_computation(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp column_status(column, state) do
    cond do
      not MapSet.member?(state.known_columns, column) -> {:error, :unknown_column}
      match?({:ok, _}, CacheOwner.lookup(column)) -> CacheOwner.lookup(column)
      computing_isolation?(column) or not Queue.processed?(state.queue, column) -> {:error, :pending}
      true -> {:error, :failed}
    end
  end

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

  defp known_columns(data_sources), do: Enum.flat_map(data_sources, &data_source_columns/1)

  defp data_source_columns(data_source), do: Enum.flat_map(data_source.tables, &table_columns(data_source, &1))

  defp table_columns(_data_source, {_table_id, %{auto_isolating_column_classification: false}}), do: []

  defp table_columns(data_source, {_table_id, table}) do
    table.columns
    |> Enum.reject(&Map.has_key?(table.isolating_columns, &1.name))
    |> Enum.map(&{data_source.name, table.name, &1.name})
  end

  defp add_waiting_request(state, column, from) do
    queue = if computing_isolation?(column), do: state.queue, else: Queue.set_high_priority(state.queue, column)
    waiting = Map.update(state.waiting, column, [from], &[from | &1])
    %{state | waiting: waiting, queue: queue}
  end

  defp computing_isolation?(column),
    do: match?({:ok, %{column: ^column}}, Parent.GenServer.child_meta(:compute_isolation_job))

  defp respond_error_on_missing_columns(state) do
    {good, missing} =
      Enum.split_with(state.waiting, fn {column, _clients} -> MapSet.member?(state.known_columns, column) end)

    Enum.each(missing, fn {_column, clients} -> Enum.each(clients, &GenServer.reply(&1, {:error, :unknown_column})) end)
    %{state | waiting: Map.new(good)}
  end

  defp enqueue_next_refresh(), do: Process.send_after(self(), :refresh, @refresh_interval)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(opts \\ []) do
    opts =
      [
        columns_provider: &known_columns/1,
        compute_isolation_fun: &compute_column_isolation/1,
        registered?: true,
        auto_refresh?: true
      ]
      |> Keyword.merge(opts)
      |> Map.new()

    gen_server_opts = if opts.registered?, do: [name: __MODULE__], else: []
    Parent.GenServer.start_link(__MODULE__, opts, gen_server_opts)
  end
end
