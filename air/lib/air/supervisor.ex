defmodule Air.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      Air.Repo,
      Air.Repo.Migrator,
      Air.Service.Cloak,
      Air.Service.DataSource,
      Air.Service.View,
      Air.Service.Query,
      Air.Service.Central,
      Air.ApiTokenTimestampUpdater,
      Air.Endpoint,
      Air.MonitoringEndpoint,
      Air.BOM,
      Air.PsqlServer,
      Air.PsqlServer.ConnectionRegistry,
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Air.Supervisor)
  end
end
