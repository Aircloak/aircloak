defmodule Air.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      Air.ResultProcessor.supervisor_spec(),
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      worker(Air.QueryEvents, []),
      worker(Air.DataSourceManager, []),
      supervisor(Task.Supervisor, [[name: Air.ApiTokenTimestampUpdater]], [id: :api_token_updater]),
      worker(Air.Monitoring.FailedQueries, []),
      Air.ResultProcessor.observer_spec(),
      worker(Air.Endpoint, []),
      worker(Air.BOM, []),
      Air.PsqlServer.RanchServer.supervisor_spec(8432)
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Air.Supervisor)
  end
end
