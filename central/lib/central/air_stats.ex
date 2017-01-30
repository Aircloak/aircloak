defmodule Central.AirStats do
  @moduledoc "Starts processes for gathering and logging of air statistics."


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the supervisor of connection monitors."
  @spec start_link() :: Supervisor.on_start
  def start_link() do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [
        supervisor(Task.Supervisor, [[name: Central.AirStats.TaskSup]]),
        worker(Central.AirStats.PeriodicLogger, []),
      ],
      strategy: :rest_for_one
    )
  end

  @doc "Should be started from the air channel process to monitor the connection."
  @spec register(Central.Schemas.Customer.t, String.t) :: :ok
  def register(customer, air_name) do
    update_air_status(customer, air_name, :online)
    Aircloak.ProcessMonitor.on_exit(fn -> update_air_status(customer, air_name, :offline) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_air_status(customer, air_name, status) do
    Task.Supervisor.start_child(Central.AirStats.TaskSup, Central.Service.Customer, :update_air_status,
      [customer, air_name, status])

    Task.Supervisor.start_child(Central.AirStats.TaskSup, Central.Service.ElasticSearch, :record_air_presence,
      [customer, air_name, status])
  end
end
