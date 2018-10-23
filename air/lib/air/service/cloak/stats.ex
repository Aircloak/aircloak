defmodule Air.Service.Cloak.Stats do
  @moduledoc """
  The Cloak stats service stores metrics about memory and query runs for each online cloak.
  """
  require Logger

  alias Air.Service.Cloak.Stats.Internal

  use GenServer

  @max_allowed_idle_periods 10

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Records cloak memory readings"
  @spec record_memory(Internal.cloak_id(), Internal.raw_memory_reading()) :: :ok
  def record_memory(cloak_id, reading), do: GenServer.cast(__MODULE__, {:record_memory, cloak_id, reading})

  @doc "Records that a query was executed on a cloak"
  @spec record_query(Internal.cloak_id()) :: :ok
  def record_query(cloak_id), do: GenServer.cast(__MODULE__, {:record_query, cloak_id})

  @doc "Returns memory stats for all Cloaks"
  @spec cloak_stats() :: Internal.cloak_stats()
  def cloak_stats(), do: GenServer.call(__MODULE__, :stats)

  @doc "Returns memory stats for all Cloaks"
  @spec cloak_stats(Internal.cloak_id()) :: Internal.stats()
  def cloak_stats(cloak_id), do: GenServer.call(__MODULE__, {:stats, cloak_id})

  @doc "Triggers aggregation of stats"
  @spec aggregate() :: :ok
  def aggregate(), do: GenServer.cast(__MODULE__, :aggregate)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_),
    do:
      {:ok,
       %{
         metrics: Internal.new(),
         cloak_presence: %{seen_in_reporting_interval: MapSet.new(), last_seen: Map.new()}
       }}

  @impl GenServer
  def handle_cast({:record_memory, cloak_id, memory_reading}, state),
    do: {:noreply, record_metric(state, cloak_id, &Internal.record_memory(&1, &2, memory_reading))}

  def handle_cast({:record_query, cloak_id}, state),
    do: {:noreply, record_metric(state, cloak_id, &Internal.record_query(&1, &2))}

  def handle_cast(:aggregate, state) do
    last_seen = state.cloak_presence.last_seen

    cloaks_with_reports = MapSet.to_list(state.cloak_presence.seen_in_reporting_interval)
    existing_cloaks = Map.keys(last_seen)
    new_cloaks = cloaks_with_reports -- existing_cloaks
    cloaks_without_reports = existing_cloaks -- cloaks_with_reports

    expired_cloaks =
      last_seen
      |> Enum.filter(fn {_cloak_id, periods_since_seen} -> periods_since_seen >= @max_allowed_idle_periods end)
      |> Enum.map(fn {cloak_id, _periods_since_seen} -> cloak_id end)

    updated_last_seen =
      last_seen
      |> flipped_reduce(cloaks_with_reports, &Map.put(&2, &1, 0))
      |> flipped_reduce(
        cloaks_without_reports,
        &Map.update!(&2, &1, fn periods_since_seen -> periods_since_seen + 1 end)
      )
      |> flipped_reduce(expired_cloaks, &Map.delete(&2, &1))

    state =
      state
      |> update_in([:metrics], fn metrics ->
        metrics
        |> flipped_reduce(new_cloaks, &Internal.initialize(&2, &1))
        |> flipped_reduce(expired_cloaks, &Internal.remove(&2, &1))
        |> Internal.aggregate()
      end)
      |> put_in([:cloak_presence, :seen_in_reporting_interval], MapSet.new())
      |> put_in([:cloak_presence, :last_seen], updated_last_seen)

    {:noreply, state}
  end

  @impl GenServer
  def handle_call(:stats, _from, state),
    do: {:reply, %{state | metrics: Internal.cloak_stats(state.metrics)}, state}

  @impl GenServer
  def handle_call({:stats, cloak_id}, _from, state),
    do: {:reply, Internal.cloak_stats(state.metrics)[cloak_id], state}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp flipped_reduce(initial_acc, enumerable, function), do: Enum.reduce(enumerable, initial_acc, function)

  defp record_metric(state, cloak_id, callback),
    do:
      state
      |> update_in([:metrics], &callback.(&1, cloak_id))
      |> update_in([:cloak_presence, :seen_in_reporting_interval], &MapSet.put(&1, cloak_id))
end
