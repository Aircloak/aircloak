defmodule Air.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Air.DataSourceManager, []),
      Air.ResultProcessor.supervisor_spec(),
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      supervisor(Air.QueryEvents, []),
      supervisor(Task.Supervisor, [[name: Air.ApiTokenTimestampUpdater]], [id: :api_token_updater]),
      worker(Air.Monitoring.FailedQueries, []),
      Air.ResultProcessor.observer_spec(),
      worker(Air.Endpoint, []),
      worker(Air.MonitoringEndpoint, []),
      worker(Air.BOM, []),
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

      [
        worker(Air.CentralSocket, []),
        Air.CentralQueryReporter.supervisor_spec(),
        Air.CentralQueryReporter.observer_spec(),
      ]
    end
  end
end
