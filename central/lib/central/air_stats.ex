defmodule Central.AirStats do
  @moduledoc "Starts processes for gathering and logging of air statistics."

  use Aircloak.ChildSpec.Supervisor
  alias Central.Service.Customer

  @type air_info :: %{
    air_version: String.t,
    online_cloaks: [%{
      name: String.t,
      data_source_names: [String.t],
      version: String.t,
    }],
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the supervisor of connection monitors."
  @spec start_link() :: Supervisor.on_start
  def start_link(), do:
    Supervisor.start_link(
      [
        Aircloak.ChildSpec.task_supervisor(name: Central.AirStats.TaskSup),
        Central.AirStats.PeriodicLogger,
      ],
      strategy: :rest_for_one
    )

  @doc "Should be started from the air channel process to monitor the connection."
  @spec register(Central.Schemas.Customer.t, String.t, air_info) :: :ok
  def register(customer, air_name, air_info) do
    # log information in a separate task to prevent blocking or crashing the main channel process
    Task.Supervisor.start_child(Central.AirStats.TaskSup, Customer, :mark_air_online,
      [customer, air_name, air_info])
    Aircloak.ProcessMonitor.on_exit(fn -> Customer.mark_air_offline(customer, air_name) end)
  end
end
