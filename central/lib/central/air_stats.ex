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
  @spec register(Central.Schemas.Customer.t, String.t, [String.t]) :: :ok
  def register(customer, air_name, online_cloaks) do
    mark_air_online(customer, air_name, online_cloaks)
    Aircloak.ProcessMonitor.on_exit(fn -> mark_air_offline(customer, air_name) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp mark_air_online(customer, air_name, online_cloaks) do
    start_task(Central.Service.Customer, :mark_air_online, [customer, air_name, online_cloaks])
    start_task(Central.Service.ElasticSearch, :record_air_presence, [customer, air_name, :online])
  end

  defp mark_air_offline(customer, air_name) do
    start_task(Central.Service.Customer, :mark_air_offline, [customer, air_name])
    start_task(Central.Service.ElasticSearch, :record_air_presence, [customer, air_name, :offline])
  end

  defp start_task(module, fun, args), do:
    Task.Supervisor.start_child(Central.AirStats.TaskSup, module, fun, args)
end
