defmodule Cloak.DataSource.Isolators.CacheOwner do
  @moduledoc "Owner of the isolators cache ets table."

  use Parent.GenServer
  import Aircloak, only: [in_env: 1]
  alias Cloak.DataSource.Isolators.Queue

  # Version of the persisted cache format. Bump this if you're changing the format.
  @persisted_cache_version 1

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Performs a lookup into the isolator cache table."
  @spec lookup(Queue.column()) :: {:ok, boolean} | :error
  def lookup(column) do
    case :ets.match(__MODULE__, {column, :"$1"}) do
      [[isolates?]] -> {:ok, isolates?}
      [] -> :error
    end
  end

  @doc "Stores an item into the isolator cache table."
  @spec store(Queue.column(), boolean) :: :ok
  def store(column, isolated) do
    :ets.insert(__MODULE__, {column, isolated})
    signal_change()
    :ok
  end

  @doc "Deletes unkown columns from the cache table."
  @spec remove_unknown_columns(MapSet.t()) :: :ok
  def remove_unknown_columns(known_columns) do
    cached_columns() |> MapSet.difference(known_columns) |> Enum.each(&:ets.delete(__MODULE__, &1))
    signal_change()
  end

  @doc "Returns the collection of cached columns."
  @spec cached_columns() :: MapSet.t()
  def cached_columns(), do: :ets.match(__MODULE__, {:"$1", :_}) |> Enum.concat() |> MapSet.new()

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_arg) do
    :ets.new(__MODULE__, [:named_table, :public, :set, read_concurrency: true])
    start_cache_restore_job()
    {:ok, %{changed?: false}}
  end

  @impl GenServer
  def handle_cast(:signal_change, state), do: {:noreply, maybe_start_persist_job(%{state | changed?: true})}

  @impl Parent.GenServer
  def handle_child_terminated(:persist_job, _meta, _pid, _reason, state), do: {:noreply, maybe_start_persist_job(state)}
  def handle_child_terminated(:restore_job, _meta, _pid, _reason, state), do: {:noreply, maybe_start_persist_job(state)}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp signal_change(), do: GenServer.cast(__MODULE__, :signal_change)

  defp maybe_start_persist_job(%{changed?: false} = state), do: state

  defp maybe_start_persist_job(%{changed?: true} = state) do
    if Parent.GenServer.child?(:restore_job) or Parent.GenServer.child?(:persist_job) do
      state
    else
      start_persist_job()
      %{state | changed?: false}
    end
  end

  defp start_persist_job() do
    cache_contents = :ets.tab2list(__MODULE__)

    Parent.GenServer.start_child(%{
      id: :persist_job,
      start: {Task, :start_link, [fn -> persist_cache(cache_contents) end]}
    })
  end

  defp persist_cache(cache_contents) do
    File.mkdir_p!(Path.dirname(cache_file()))

    # Note: if you're changing the cache format, please bump `@persisted_cache_version`. This will ensure that the
    # next version ignores the persisted cache of the previous version.
    File.write!(cache_file(), :erlang.term_to_binary({__MODULE__, @persisted_cache_version, cache_contents}))
  end

  defp start_cache_restore_job() do
    # Starting the restore job as a background process allows us to ignore restore failures and implement a finite time
    # synchronism (waiting for the cache to be restored for some time, but not forever).
    Parent.GenServer.start_child(%{
      id: :restore_job,
      start: {Task, :start_link, [fn -> restore_cache() end]}
    })

    # We'll wait a bit for the cache to be restored. This improves synchronous behaviour, allowing us to skip initial
    # cache priming if we're able to restore it.
    Parent.GenServer.await_child_termination(:restore_job, :timer.seconds(10))
  end

  defp restore_cache() do
    with {:ok, serialized_cache} <- File.read(cache_file()),
         {__MODULE__, @persisted_cache_version, cache_contents} <- :erlang.binary_to_term(serialized_cache),
         do: Enum.each(cache_contents, &:ets.insert(__MODULE__, &1))
  end

  @doc false
  def cache_file(), do: Path.join(~w(#{Application.app_dir(:cloak)} priv persist #{cache_file_name()}))

  # using env suffix in dev/test to avoid clashes of persisted caches between different environments
  defp cache_file_name(), do: in_env(prod: "isolators_cache", else: "isolators_cache_#{unquote(Mix.env())}")

  # -------------------------------------------------------------------
  # Supervison tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(_arg), do: Parent.GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
