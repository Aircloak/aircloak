defmodule Air.Supervisor do
  @moduledoc false

  import Aircloak, only: [in_env: 1]

  def(start_link) do
    Supervisor.start_link(
      [
        Air.Repo,
        Air.Repo.Migrator,
        Air.Service.Settings,
        Air.Service.Cloak,
        Air.Service.DataSource,
        Air.Service.View,
        Air.Service.Query,
        Air.Service.License,
        Air.Service.Central,
        Air.Service.User,
        Air.Service.Export,
        Air.Service.ShadowDb,
        Air.Service.Cleanup,
        in_env(test: nil, else: Air.Service.LDAP.PeriodicSync),
        Air.ApiTokenTimestampUpdater,
        Air.Web,
        AirWeb.MonitoringEndpoint,
        Air.BOM,
        Air.PsqlServer,
        Air.PsqlServer.ConnectionRegistry
      ]
      |> Enum.reject(&is_nil/1),
      strategy: :one_for_one,
      name: Air.Supervisor
    )
  end
end
