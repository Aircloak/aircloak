defmodule Central.AirStats.PeriodicLogger do
  @moduledoc "Periodic logging of data for all known airs."

  use GenServer, start: {__MODULE__, :start_link, []}
  alias Central.Service.{Customer, ElasticSearch}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the periodic logger process."
  @spec start_link() :: GenServer.on_start
  def start_link(), do:
    GenServer.start_link(__MODULE__, nil)


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(nil) do
    queue_next_logging()
    {:ok, nil}
  end

  @doc false
  def handle_info({:DOWN, mref, :process, _, _}, mref) do
    queue_next_logging()
    {:noreply, nil}
  end
  def handle_info(:start_logger, nil), do:
    {:noreply, start_logger()}
  def handle_info(_, state), do:
    {:noreply, state}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp queue_next_logging(), do:
    Process.send_after(self(), :start_logger, Application.fetch_env!(:central, :air_status_logging_interval))

  defp start_logger() do
    {:ok, pid} = Task.Supervisor.start_child(Central.AirStats.TaskSup, &log_connected_airs/0)
    Process.monitor(pid)
  end

  defp log_connected_airs(), do:
    Enum.each(Customer.airs(), &ElasticSearch.record_air_presence/1)
end
