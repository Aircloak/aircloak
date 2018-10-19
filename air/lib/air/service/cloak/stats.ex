defmodule Air.Service.Cloak.Stats do
  @moduledoc """
  The Cloak service holds metadata about cloaks and their datastores as well as facilities
  for registering them with the database backing the air system.
  """
  require Logger

  alias Air.Service.Cloak.Stats.Internal

  use GenServer

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Records cloak memory readings"
  @spec record_memory(Internal.cloak_id(), Internal.raw_memory_reading()) :: :ok
  def record_memory(cloak_id, reading), do: GenServer.cast(__MODULE__, {:record_memory, cloak_id, reading})

  @doc "Returns memory stats for all Cloaks"
  @spec cloak_stats() :: Internal.stats()
  def cloak_stats(), do: GenServer.call(__MODULE__, :stats)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    schedule_processing()
    {:ok, Internal.initial_state()}
  end

  @impl GenServer
  def handle_cast({:record_memory, cloak_id, memory_reading}, state),
    do: {:noreply, Internal.record_memory(state, cloak_id, memory_reading)}

  @impl GenServer
  def handle_call(:stats, _from, state),
    do: {:reply, Internal.cloak_stats(state), state}

  @impl GenServer
  def handle_info(:process_stats, state) do
    schedule_processing()
    processed_state = Internal.process(state)
    {:noreply, processed_state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp schedule_processing(), do: Process.send_after(self(), :process_stats, :timer.seconds(1))
end
