defmodule Cloak.DataSource.PerColumn.Cache do
  @moduledoc "Implementation of the cache which holds a property for all known columns of all data sources."

  use Parent.GenServer
  require Logger
  alias Cloak.DataSource.PerColumn.{PersistentKeyValue, Queue, Descriptor, Result}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the value of the property for the given column."
  @spec value(GenServer.server(), Cloak.DataSource.t(), String.t(), String.t()) :: any
  def value(server, data_source, table_name, column_name) do
    column = {data_source, table_name, column_name}

    with {:error, :failed, name, default} <- GenServer.call(server, {:compute_value, column}, :infinity) do
      Logger.error("#{inspect(name)} failed for `#{table_name}`.`#{column_name}`")
      default
    else
      {:ok, property} -> property
      {:error, :unknown_column} -> raise "Unknown column `#{table_name}`.`#{column_name}`"
    end
  end

  @doc "Performs a cache lookup."
  @spec lookup(GenServer.server(), Cloak.DataSource.t(), String.t(), String.t()) ::
          {:ok, any} | {:error, :pending | :failed | :unknown_column}
  def lookup(server, data_source, table_name, column_name) do
    case GenServer.call(server, {:column_status, Descriptor.hash(data_source, table_name, column_name)}) do
      {:error, :failed, _, _} -> {:error, :failed}
      other -> other
    end
  end

  @doc "Updates the cache with a result computed in another Cloak."
  @spec update_with_remote_result(GenServer.server(), %{
          descriptor: Descriptor.t(),
          status: :ok,
          expires: NaiveDateTime.t(),
          result: any,
          type: Result.result_type()
        }) :: :ok
  def update_with_remote_result(server, result) do
    GenServer.cast(server, {:update_with_remote_result, Result.decrypt(result)})
  end

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(opts) do
    if opts.auto_refresh?, do: Cloak.DataSource.subscribe_to_changes()
    :timer.send_interval(:timer.hours(1), :refresh)

    descriptor_to_column_map =
      Cloak.DataSource.all()
      |> opts.columns_provider.()
      |> descriptor_map()

    queue =
      descriptor_to_column_map
      |> Map.keys()
      |> Enum.shuffle()
      |> Queue.new(PersistentKeyValue.cached_columns(opts.cache_owner))

    state = %{
      default: opts.default,
      cache_owner: opts.cache_owner,
      descriptor_to_column_map: descriptor_to_column_map,
      queue: queue,
      waiting: %{},
      opts: opts
    }

    {:ok, start_next_computation(state)}
  end

  @impl GenServer
  def handle_call({:compute_value, column}, from, state) do
    descriptor = Descriptor.hash(column)

    case column_status(descriptor, state) do
      {:ok, result} -> {:reply, {:ok, result}, state}
      {:error, :pending} -> {:noreply, add_waiting_request(state, descriptor, from)}
      error -> {:reply, error, state}
    end
  end

  def handle_call({:column_status, column}, _from, state) do
    {:reply, column_status(column, state), state}
  end

  @impl GenServer
  def handle_cast({:update_with_remote_result, result}, state) do
    if Queue.member?(state.queue, result.descriptor) do
      PersistentKeyValue.store(state.cache_owner, result.descriptor, result.result, result.expires)
      {:noreply, update_in(state.queue, &Queue.make_processed(&1, result.descriptor, result.expires))}
    else
      {:noreply, state}
    end
  end

  @impl GenServer
  def handle_info(:refresh, state) do
    # Refresh is handled by the queue (see `Queue.refresh/1` for details), which means that the previously
    # processed columns are moved to the back of the queue so long as their expiry elapsed.
    # This gives us a simple solution to the overload problem. If we can't compute all columns during the refresh
    # interval, we'll end up constantly refreshing, but tail columns won't starve, and no queue will grow indefinitely.
    state = maybe_start_next_computation(update_in(state.queue, &Queue.refresh/1))
    {:noreply, state}
  end

  def handle_info({:data_sources_changed, new_data_sources}, state) do
    descriptor_to_column_map =
      new_data_sources
      |> state.opts.columns_provider.()
      |> descriptor_map()

    known_column_descriptors = Map.keys(descriptor_to_column_map)

    PersistentKeyValue.remove_unknown_columns(state.cache_owner, known_column_descriptors)

    state = %{state | descriptor_to_column_map: descriptor_to_column_map}

    state = update_in(state.queue, &Queue.update_known_columns(&1, known_column_descriptors))
    state = respond_error_on_missing_columns(state)
    {:noreply, maybe_start_next_computation(state)}
  end

  @impl Parent.GenServer
  def handle_child_terminated(:compute_job, meta, _pid, _reason, state) do
    result =
      case PersistentKeyValue.lookup(state.cache_owner, meta.descriptor) do
        :error -> failed(state)
        {:ok, result} -> {:ok, result}
      end

    state.waiting |> Map.get(meta.descriptor, []) |> Enum.each(&GenServer.reply(&1, result))
    {:noreply, start_next_computation(state)}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp column_status(descriptor, state) do
    cond do
      not Map.has_key?(state.descriptor_to_column_map, descriptor) ->
        {:error, :unknown_column}

      match?({:ok, _}, PersistentKeyValue.lookup(state.cache_owner, descriptor)) ->
        PersistentKeyValue.lookup(state.cache_owner, descriptor)

      computing?(descriptor) or not Queue.processed?(state.queue, descriptor) ->
        {:error, :pending}

      true ->
        failed(state)
    end
  end

  defp failed(state), do: {:error, :failed, state.opts.name, state.default}

  defp maybe_start_next_computation(state) do
    if Parent.GenServer.child?(:compute_job), do: state, else: start_next_computation(state)
  end

  defp start_next_computation(state) do
    with {descriptor, queue} <- Queue.next_column(state.queue, expiry(state)),
         {:ok, column} <- Map.fetch(state.descriptor_to_column_map, descriptor) do
      Parent.GenServer.start_child(%{
        id: :compute_job,
        start: fn -> start_compute(column, state) end,
        meta: %{descriptor: descriptor}
      })

      %{state | queue: queue}
    else
      _ -> state
    end
  end

  defp start_compute(column, state) do
    Task.start_link(fn ->
      Logger.debug(fn -> "#{inspect(state.opts.name)} computing for #{inspect(column)}" end)
      property = state.opts.property_fun.(column)
      expires = expiry(state)
      descriptor = Descriptor.hash(column)
      PersistentKeyValue.store(state.cache_owner, descriptor, property, expires)
    end)
  end

  defp add_waiting_request(state, column, from) do
    queue = if computing?(column), do: state.queue, else: Queue.set_high_priority(state.queue, column)
    waiting = Map.update(state.waiting, column, [from], &[from | &1])
    %{state | waiting: waiting, queue: queue}
  end

  defp computing?(column), do: match?({:ok, %{descriptor: ^column}}, Parent.GenServer.child_meta(:compute_job))

  defp respond_error_on_missing_columns(state) do
    {good, missing} =
      Enum.split_with(state.waiting, fn {column, _clients} -> Map.has_key?(state.descriptor_to_column_map, column) end)

    Enum.each(missing, fn {_column, clients} -> Enum.each(clients, &GenServer.reply(&1, {:error, :unknown_column})) end)
    %{state | waiting: Map.new(good)}
  end

  defp expiry(state), do: NaiveDateTime.utc_now() |> NaiveDateTime.add(state.opts.refresh_interval, :millisecond)

  defp descriptor_map(columns) do
    columns
    |> Enum.map(&{Descriptor.hash(&1), &1})
    |> Enum.into(%{})
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(opts \\ []) do
    opts =
      [
        registered?: true,
        auto_refresh?: true
      ]
      |> Keyword.merge(opts)
      |> Map.new()

    gen_server_opts = if opts.registered?, do: [name: opts.name], else: []
    Parent.GenServer.start_link(__MODULE__, opts, gen_server_opts)
  end
end
