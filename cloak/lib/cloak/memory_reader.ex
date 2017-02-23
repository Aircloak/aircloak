defmodule Cloak.MemoryReader do
  @moduledoc """
  Periodically reads the memory available on the server in order to
  determine if we are about to run out of memory or not.

  Once the free memory drops below a certain threshold, it will
  start looking at projections of future memory developments.
  If the memory is deemed to shortly reach critical levels,
  running queries will be cancelled.
  """

  use GenServer

  alias Cloak.MemoryReader.MemoryProjector

  require Logger

  @memory_check_interval 100
  @limit_to_start_checks 500_000 # 500Mb
  @limit_to_check_for 100_000 # 100Mb
  @allowed_minimum_time_to_limit 3_000 # 3s


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the memory reader server"
  @spec start_link() :: GenServer.on_start
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc "Registers a query such that it can later be killed in case of a low memory event"
  @spec register_query() :: :ok
  def register_query(), do:
    GenServer.cast(__MODULE__, {:register_query, self()})


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    schedule_check()
    {:ok, %{
      memory_projector: MemoryProjector.new(),
      queries: [],
    }}
  end

  @doc false
  def handle_cast({:register_query, pid}, %{queries: queries} = state) do
    Process.monitor(pid)
    {:noreply, %{state | queries: [pid | queries]}}
  end

  @doc false
  def handle_info({:DOWN, _monitor_ref, :process, pid, _info}, %{queries: queries} = state), do:
    {:noreply, %{state | queries: Enum.reject(queries, & &1 == pid)}}
  def handle_info(:read_memory, state) do
    pid = self()
    Task.start_link(fn() ->
      reading = :memsup.get_system_memory_data()
      send(pid, {:record_memory_reading, reading})
    end)
    schedule_check()
    {:noreply, state}
  end
  def handle_info({:record_memory_reading, reading}, %{memory_projector: projector} = state) do
    time = System.monotonic_time(:millisecond)
    free_memory = Keyword.get(reading, :free_memory)
    projector = MemoryProjector.add_reading(projector, free_memory, time)
    state = %{state | memory_projector: projector}
    perform_memory_check(free_memory, state)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp perform_memory_check(free_memory, state) when free_memory > @limit_to_start_checks, do:
    {:noreply, state}
  defp perform_memory_check(free_memory, %{memory_projector: projector} = state) do
    case MemoryProjector.time_until_limit(projector, @limit_to_check_for) do
      {:ok, time} when time <= @allowed_minimum_time_to_limit ->
        Logger.error("Anticipating reaching low memory threshold (#{to_mb(@limit_to_check_for)} Mb) " <>
          "in #{to_sec(time)} seconds. Free memory: #{to_mb(free_memory)} Mb")
        kill_query(state)
      _ -> {:noreply, state}
    end
  end

  defp to_mb(kb), do: trunc(kb / 1024)

  defp to_sec(ms), do: max(trunc(ms / 1000), 0)

  defp kill_query(%{queries: []} = state) do
    Logger.warn("No queries to abort")
    {:noreply, state}
  end
  defp kill_query(%{queries: [query | queries]} = state) do
    Cloak.Query.Runner.stop(query, :oom)
    {:noreply, %{state | queries: queries}}
  end

  defp schedule_check(), do: Process.send_after(self(), :read_memory, @memory_check_interval)
end
