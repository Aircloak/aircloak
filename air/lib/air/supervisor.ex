defmodule Air.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Air.Service.Cloak, []),
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      supervisor(Air.QueryEvents, []),
      supervisor(Task.Supervisor, [[name: Air.ApiTokenTimestampUpdater]], [id: :api_token_updater]),
      worker(Air.Monitoring.FailedQueries, []),
      Air.QueryLifecycle.observer_spec(),
      worker(Air.Endpoint, []),
      worker(Air.MonitoringEndpoint, []),
      worker(Air.BOM, []),
      worker(Air.Service.Central.CallsQueue, []),
      Air.PsqlServer.child_spec()
    ] ++ system_processes()

    Supervisor.start_link(children, strategy: :one_for_one, name: Air.Supervisor)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  if Mix.env == :test do
    defp system_processes, do: []
  else
    # Processes which we don't want to start in the test environment
    defp system_processes do
      import Supervisor.Spec, warn: false
      [supervisor(Air.CentralClient, [])]
    end
  end
end
