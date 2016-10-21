defmodule Air.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      Air.ResultProcessor.supervisor_spec(),
      Air.CentralQueryReporter.supervisor_spec(),
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      worker(Air.QueryEvents, []),
      worker(Air.DataSourceManager, []),
      supervisor(Task.Supervisor, [[name: Air.ApiTokenTimestampUpdater]], [id: :api_token_updater]),
      worker(Air.Monitoring.FailedQueries, []),
      Air.ResultProcessor.observer_spec(),
      Air.CentralQueryReporter.observer_spec(),
      worker(Air.Endpoint, []),
      worker(Air.BOM, []),
      Air.PsqlServer.RanchServer.supervisor_spec()
    ] ++ system_processes()

    Supervisor.start_link(children, strategy: :one_for_one, name: Air.Supervisor)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  unless Mix.env == :test do
    # Processes which we don't want to start in the test environment
    defp system_processes do
      import Supervisor.Spec, warn: false

      [
        worker(Air.CentralSocket, [])
      ]
    end
  else
    defp system_processes, do: []
  end
end
