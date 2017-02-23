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

  alias Cloak.MemoryReader.{MemInfo, MemoryProjector}

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
  def start_link(), do: GenServer.start_link(__MODULE__, [])


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    :timer.send_interval(@memory_check_interval, self(), :read_memory)
    {:ok, %{memory_projector: MemoryProjector.new(), memory_reader: memory_reader()}}
  end

  @doc false
  def handle_info(:read_memory, %{memory_reader: nil} = state) do
    {:noreply, state}
  end
  def handle_info(:read_memory, %{memory_projector: projector, memory_reader: reader} = state) do
    %MemInfo{free_memory: free_memory} = reader.read()
    updated_projector = MemoryProjector.add_reading(projector, free_memory, System.monotonic_time(:millisecond))

    if free_memory < @limit_to_start_checks do
      case MemoryProjector.time_until_limit(updated_projector, @limit_to_check_for) do
        :infinity -> :ok
        {:ok, time} ->
          if time <= @allowed_minimum_time_to_limit do
            seconds = trunc(time / 1000)
            Logger.error("Anticipating running out of memory in #{seconds} seconds. Free memory: #{free_memory}kB")
          end
      end
    end
    {:noreply, %{state | memory_projector: updated_projector}}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp has_proc_meminfo?(), do: File.exists?("/proc/meminfo")

  defp has_vm_stat?(), do: File.exists?("/usr/bin/vm_stat")

  defp memory_reader() do
    cond do
      has_proc_meminfo?() ->
        Logger.debug("Memory reader using /proc/meminfo as memory source")
        Cloak.MemoryReader.ProcMeminfoReader
      has_vm_stat?() ->
        Logger.debug("Memory reader using /usr/bin/vm_stat as memory source")
        Cloak.MemoryReader.VMStatReader
      true ->
        Logger.warn("Can't find a memory reader")
        nil
    end
  end
end
