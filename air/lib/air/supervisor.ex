defmodule Air.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      Air.Service.Cloak.supervisor_spec(),
      Air.Service.DataSource.supervisor_spec(),
      Air.Service.View.supervisor_spec(),
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      supervisor(Air.QueryEvents, []),
      supervisor(Task.Supervisor, [[name: Air.ApiTokenTimestampUpdater]], [id: :api_token_updater]),
      worker(Air.Monitoring.FailedQueries, []),
      Air.QueryLifecycle.observer_spec(),
      worker(Air.Endpoint, []),
      worker(Air.MonitoringEndpoint, []),
      worker(Air.BOM, []),
      Air.Service.Central.supervisor_spec(),
      Air.PsqlServer.child_spec()
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Air.Supervisor)
  end
end
