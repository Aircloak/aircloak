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
         cloak_presence: %{seen_in_reporting_interval: MapSet.new(), unseen_periods: Map.new()}
       }}

  @impl GenServer
  def handle_cast({:record_memory, cloak_id, memory_reading}, state),
    do: {:noreply, record_metric(state, cloak_id, &Internal.record_memory(&1, &2, memory_reading))}

  def handle_cast({:record_query, cloak_id}, state),
    do: {:noreply, record_metric(state, cloak_id, &Internal.record_query(&1, &2))}

  def handle_cast(:aggregate, state) do
    unseen_periods = state.cloak_presence.unseen_periods

    cloaks_with_reports = MapSet.to_list(state.cloak_presence.seen_in_reporting_interval)
    existing_cloaks = Map.keys(unseen_periods)
    new_cloaks = cloaks_with_reports -- existing_cloaks
    cloaks_without_reports = existing_cloaks -- cloaks_with_reports

    expired_cloaks =
      unseen_periods
      |> Enum.filter(fn {_cloak_id, periods_since_seen} -> periods_since_seen >= @max_allowed_idle_periods end)
      |> Enum.map(fn {cloak_id, _periods_since_seen} -> cloak_id end)

    updated_last_seen =
      unseen_periods
      |> reset_seen_cloaks(cloaks_with_reports)
      |> increment_unseen_cloaks(cloaks_without_reports)
      |> remove_expired(expired_cloaks)

    state =
      state
      |> update_in([:metrics], fn metrics ->
        metrics
        |> initialize(new_cloaks)
        |> remove(expired_cloaks)
        |> Internal.aggregate()
      end)
      |> put_in([:cloak_presence, :seen_in_reporting_interval], MapSet.new())
      |> put_in([:cloak_presence, :unseen_periods], updated_last_seen)

    {:noreply, state}
  end

  @impl GenServer
  def handle_call({:stats, cloak_id}, _from, state),
    do: {:reply, Internal.cloak_stats(state.metrics)[cloak_id], state}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp remove_expired(unseen_periods, expired_cloaks),
    do: Enum.reduce(expired_cloaks, unseen_periods, &Map.delete(&2, &1))

  defp reset_seen_cloaks(unseen_periods, seen_cloaks), do: Enum.reduce(seen_cloaks, unseen_periods, &Map.put(&2, &1, 0))

  defp increment_unseen_cloaks(unseen_periods, unseen_cloaks),
    do:
      Enum.reduce(
        unseen_cloaks,
        unseen_periods,
        &Map.update!(&2, &1, fn periods_since_seen -> periods_since_seen + 1 end)
      )

  defp initialize(metrics, new_cloaks), do: Enum.reduce(new_cloaks, metrics, &Internal.initialize(&2, &1))

  defp remove(metrics, expired_cloaks), do: Enum.reduce(expired_cloaks, metrics, &Internal.remove(&2, &1))

  defp record_metric(state, cloak_id, callback),
    do:
      state
      |> update_in([:metrics], &callback.(&1, cloak_id))
      |> update_in([:cloak_presence, :seen_in_reporting_interval], &MapSet.put(&1, cloak_id))
end
