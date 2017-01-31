defmodule Central.AirStats do
  @moduledoc "Starts processes for gathering and logging of air statistics."

  alias Central.Service.Customer

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
  @spec register(Central.Schemas.Customer.t, String.t, [String.t]) :: :ok
  def register(customer, air_name, online_cloaks) do
    # log information in a separate task to prevent blocking or crashing the main channel process
    Task.Supervisor.start_child(Central.AirStats.TaskSup, Customer, :mark_air_online,
      [customer, air_name, online_cloaks])
    Aircloak.ProcessMonitor.on_exit(fn -> Customer.mark_air_offline(customer, air_name) end)
  end
end
