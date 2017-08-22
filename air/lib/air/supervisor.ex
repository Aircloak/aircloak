defmodule Air.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      Air.Service.Cloak.supervisor_spec(),
      Air.Service.DataSource.supervisor_spec(),
      Air.Service.View.supervisor_spec(),
      Air.Service.Query.supervisor_spec(),
      Air.Service.Central.supervisor_spec(),
      supervisor(Task.Supervisor, [[name: Air.ApiTokenTimestampUpdater]], [id: :api_token_updater]),
      worker(Air.Endpoint, []),
      worker(Air.MonitoringEndpoint, []),
      worker(Air.BOM, []),
      Air.PsqlServer.child_spec(),
      worker(Air.PsqlServer.BackendProcessRegistry, []),
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Air.Supervisor)
  end
end
