defmodule Cloak.DataSource.PerColumn.PersistentKeyValue do
  @moduledoc """
  A key-value store that keeps a value for each of a set of columns. The store provides in-memory reads, while writes
  are synchronously persisted to disk. The persisted data is used to initialize the cache on restart.
  """

  use GenServer
  import Aircloak, only: [in_env: 1]
  require Logger
  alias Cloak.DataSource.PerColumn.Queue

  @persist_interval :timer.seconds(10)

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

    {:ok, Map.put(opts, :dirty?, false)}
  end

  @impl GenServer
  def handle_cast(:signal_change, %{dirty?: false} = state) do
    Process.send_after(self(), :persist_cache, @persist_interval)
    {:noreply, %{state | dirty?: true}}
  end

  def handle_cast(:signal_change, %{dirty?: true} = state), do: {:noreply, state}

  @impl GenServer
  def handle_info(:persist_cache, %{dirty?: true} = state), do: {:noreply, persist_cache(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp signal_change(server), do: GenServer.cast(server, :signal_change)

  defp persist_cache(state) do
    Logger.info("Writing cache for #{state.name} ...")

    cache_file = cache_file(state.name)

    File.mkdir_p!(Path.dirname(cache_file))

    # Note: if you're changing the cache format, please bump `persisted_cache_version`. This will ensure that the
    # next version ignores the persisted cache of the previous version.
    cache_contents = :ets.tab2list(state.name)
    cache = :erlang.term_to_binary({state.name, state.persisted_cache_version, cache_contents})
    File.write!(cache_file, cache)

    %{state | dirty?: false}
  end

  defp restore_cache(name, persisted_cache_version) do
    Logger.info("Reading cache for #{name} ...")

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
    GenServer.start_link(
      __MODULE__,
      %{name: opts.name, persisted_cache_version: opts.persisted_cache_version},
      name: opts.name
    )
  end
end
