defmodule Cloak.MemoryReporter do
  @moduledoc "Periodically reports memory stats to the air."

  use GenServer


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the memory reporter"
  @spec start_link() :: GenServer.on_start
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    # NOTE: These readings will come from the memory reader once
    # that functionality exist. At that point we'll get these
    # readings for free.
    :timer.send_interval(:timer.seconds(1), :report_memory_stats)
    {:ok, %{}}
  end

  @doc false
  def handle_info(:report_memory_stats, state) do
    Cloak.AirSocket.send_memory_stats(:memsup.get_system_memory_data())
    {:noreply, state}
  end
end
