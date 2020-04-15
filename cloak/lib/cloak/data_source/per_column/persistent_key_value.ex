defmodule Cloak.DataSource.PerColumn.PersistentKeyValue do
  @moduledoc """
  A key-value store that keeps a value for each of a set of columns. The store provides in-memory reads, while writes
  are asynchronously persisted to disk. The persisted data is used to initialize the cache on restart.
  """

  use Parent.GenServer
  import Aircloak, only: [in_env: 1]
  require Logger
  alias Cloak.DataSource.PerColumn.Queue

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Performs a lookup into the cache table."
  @spec lookup(GenServer.server(), Queue.column()) :: {:ok, any} | :error
  def lookup(server, column) do
    case :ets.match(server, {column, :"$1", :_}) do
      [[value]] -> {:ok, value}
      [] -> :error
    end
  end

  @doc "Stores an item into the cache table."
  @spec store(GenServer.server(), Queue.column(), any, NaiveDateTime.t()) :: :ok
  def store(server, column, value, expires) do
    :ets.insert(server, {column, value, expires})
    signal_change(server)
    :ok
  end

  @doc "Deletes unkown columns from the cache table."
  @spec remove_unknown_columns(GenServer.server(), Enumerable.t()) :: :ok
  def remove_unknown_columns(server, known_columns) do
    cached_columns(server) |> Map.drop(known_columns) |> Enum.each(&:ets.delete(server, &1))
    signal_change(server)
  end

  @doc "Returns the collection of cached columns."
  @spec cached_columns(GenServer.server()) :: Queue.processed_columns()
  def cached_columns(server), do: :ets.match(server, {:"$1", :_, :"$2"}) |> Enum.map(&List.to_tuple/1) |> Enum.into(%{})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(opts) do
    :ets.new(opts.name, [:named_table, :public, :set, read_concurrency: true])
    restore_cache(opts.name, opts.persisted_cache_version)

    {:ok, Map.put(opts, :changed?, false)}
  end

  @impl GenServer
  def handle_cast(:signal_change, state), do: {:noreply, maybe_start_persist_job(%{state | changed?: true})}

  @impl Parent.GenServer
  def handle_child_terminated(:persist_job, _meta, _pid, _reason, state), do: {:noreply, maybe_start_persist_job(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp signal_change(server), do: GenServer.cast(server, :signal_change)

  defp maybe_start_persist_job(%{changed?: false} = state), do: state

  defp maybe_start_persist_job(%{changed?: true} = state) do
    if Parent.GenServer.child?(:persist_job) do
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

  defp restore_cache(name, persisted_cache_version) do
    Logger.info("Starting cache restoration for #{name} ...")

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
  def start_link(opts) do
    Parent.GenServer.start_link(
      __MODULE__,
      %{name: opts.name, persisted_cache_version: opts.persisted_cache_version},
      name: opts.name
    )
  end
end
