defmodule Cloak.DataSource.PerTable.CacheOwner do
  @moduledoc "Owner of a per table cache ets table."

  use Parent.GenServer
  import Aircloak, only: [in_env: 1]
  alias Cloak.DataSource.Isolators.Queue

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Performs a lookup into the cache table."
  @spec lookup(GenServer.server(), Queue.column()) :: {:ok, boolean} | :error
  def lookup(server, column) do
    case :ets.match(server, {column, :"$1"}) do
      [[value]] -> {:ok, value}
      [] -> :error
    end
  end

  @doc "Stores an item into the cache table."
  @spec store(GenServer.server(), Queue.column(), boolean) :: :ok
  def store(server, column, value) do
    :ets.insert(server, {column, value})
    signal_change(server)
    :ok
  end

  @doc "Deletes unkown columns from the cache table."
  @spec remove_unknown_columns(GenServer.server(), Queue.columns()) :: :ok
  def remove_unknown_columns(server, known_columns) do
    cached_columns(server) |> MapSet.difference(known_columns) |> Enum.each(&:ets.delete(server, &1))
    signal_change(server)
  end

  @doc "Returns the collection of cached columns."
  @spec cached_columns(GenServer.server()) :: Queue.columns()
  def cached_columns(server), do: :ets.match(server, {:"$1", :_}) |> Enum.concat() |> MapSet.new()

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(opts) do
    :ets.new(opts.name, [:named_table, :public, :set, read_concurrency: true])
    start_cache_restore_job(opts.name, opts.persisted_cache_version)

    {:ok, Map.put(opts, :changed?, false)}
  end

  @impl GenServer
  def handle_cast(:signal_change, state), do: {:noreply, maybe_start_persist_job(%{state | changed?: true})}

  @impl Parent.GenServer
  def handle_child_terminated(:persist_job, _meta, _pid, _reason, state), do: {:noreply, maybe_start_persist_job(state)}
  def handle_child_terminated(:restore_job, _meta, _pid, _reason, state), do: {:noreply, maybe_start_persist_job(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp signal_change(server), do: GenServer.cast(server, :signal_change)

  defp maybe_start_persist_job(%{changed?: false} = state), do: state

  defp maybe_start_persist_job(%{changed?: true} = state) do
    if Parent.GenServer.child?(:restore_job) or Parent.GenServer.child?(:persist_job) do
      state
    else
      start_persist_job(state)
      %{state | changed?: false}
    end
  end

  defp start_persist_job(state) do
    cache_contents = :ets.tab2list(state.name)

    Parent.GenServer.start_child(%{
      id: :persist_job,
      start: {Task, :start_link, [fn -> persist_cache(state, cache_contents) end]}
    })
  end

  defp persist_cache(state, cache_contents) do
    File.mkdir_p!(Path.dirname(cache_file(state.name)))

    # Note: if you're changing the cache format, please bump `persisted_cache_version`. This will ensure that the
    # next version ignores the persisted cache of the previous version.
    File.write!(
      cache_file(state.name),
      :erlang.term_to_binary({state.name, state.persisted_cache_version, cache_contents})
    )
  end

  defp start_cache_restore_job(name, persisted_cache_version) do
    # Starting the restore job as a background process allows us to ignore restore failures and implement a finite time
    # synchronism (waiting for the cache to be restored for some time, but not forever).
    Parent.GenServer.start_child(%{
      id: :restore_job,
      start: {Task, :start_link, [fn -> restore_cache(name, persisted_cache_version) end]}
    })

    # We'll wait a bit for the cache to be restored. This improves synchronous behaviour, allowing us to skip initial
    # cache priming if we're able to restore it.
    Parent.GenServer.await_child_termination(:restore_job, :timer.seconds(10))
  end

  defp restore_cache(name, persisted_cache_version) do
    with {:ok, serialized_cache} <- File.read(cache_file(name)),
         {^name, ^persisted_cache_version, cache_contents} <- :erlang.binary_to_term(serialized_cache),
         do: Enum.each(cache_contents, &:ets.insert(name, &1))
  end

  @doc false
  def cache_file(name), do: Path.join(~w(#{Application.app_dir(:cloak)} priv persist #{cache_file_name(name)}))

  # using env suffix in dev/test to avoid clashes of persisted caches between different environments
  defp cache_file_name(name) do
    in_env(
      prod: "cache_#{name}",
      else: "cache_#{name}_#{unquote(Mix.env())}"
    )
  end

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(name, persisted_cache_version) do
    Parent.GenServer.start_link(
      __MODULE__,
      %{name: name, persisted_cache_version: persisted_cache_version},
      name: name
    )
  end
end
