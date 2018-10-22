defmodule Air.Service.Cloak.Stats do
  @moduledoc """
  The Cloak service holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  alias Air.Service.Cloak.Stats.Internal

  use GenServer

  @reporting_interval :timer.seconds(10)

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Records cloak memory readings"
  @spec record_memory(Internal.cloak_id(), Internal.raw_memory_reading()) :: :ok
  def record_memory(cloak_id, reading), do: GenServer.cast(__MODULE__, {:record_memory, cloak_id, reading})

  @doc "Registers a cloak for stats"
  @spec register(Internal.cloak_id()) :: :ok
  def register(cloak_id), do: GenServer.cast(__MODULE__, {:register, cloak_id, self()})

  @doc "Returns memory stats for all Cloaks"
  @spec cloak_stats() :: Internal.cloak_stats()
  def cloak_stats(), do: GenServer.call(__MODULE__, :stats)

  @doc "Returns memory stats for all Cloaks"
  @spec cloak_stats(Internal.cloak_id()) :: Internal.stats()
  def cloak_stats(cloak_id), do: GenServer.call(__MODULE__, {:stats, cloak_id})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    schedule_processing()
    {:ok, %{metrics: Internal.initial_state(), monitoring: %{}}}
  end

  @impl GenServer
  def handle_cast({:record_memory, cloak_id, memory_reading}, state),
    do: {:noreply, %{state | metrics: Internal.record_memory(state.metrics, cloak_id, memory_reading)}}

  @impl GenServer
  def handle_cast({:register, cloak_id, pid}, state) do
    Process.monitor(pid)

    {:noreply,
     %{
       state
       | metrics: Internal.register(state.metrics, cloak_id),
         monitoring: Map.put(state.monitoring, pid, cloak_id)
     }}
  end

  @impl GenServer
  def handle_call(:stats, _from, state),
    do: {:reply, %{state | metrics: Internal.cloak_stats(state.metrics)}, state}

  @impl GenServer
  def handle_call({:stats, cloak_id}, _from, state),
    do: {:reply, Internal.cloak_stats(state.metrics)[cloak_id], state}

  @impl GenServer
  def handle_info(:process_stats, state) do
    schedule_processing()
    Task.start(&push_updated_cloak_infos/0)
    {:noreply, %{state | metrics: Internal.process(state.metrics)}}
  end

  def handle_info({:DOWN, _ref, :process, pid, _reason}, state) do
    cloak_id = Map.get(state.monitoring, pid)

    {:noreply,
     %{state | metrics: Internal.unregister(state.metrics, cloak_id), monitoring: Map.delete(state.monitoring, pid)}}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp schedule_processing(), do: Process.send_after(self(), :process_stats, @reporting_interval)

  defp push_updated_cloak_infos(), do: AirWeb.Socket.Frontend.CloakStatsChannel.broadcast_cloak_stats()
end
