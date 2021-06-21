defmodule Air.Supervisor do
  @moduledoc false

  import Aircloak, only: [in_env: 1]

  def start_link() do
    Supervisor.start_link(
      [
        Air.Repo,
        Air.Repo.Migrator,
        Air.Service.Salts,
        Air.Service.Settings,
        Air.Service.Cloak,
        Air.Service.DataSource,
        Air.Service.AnalystTable,
        Air.Service.View,
        Air.Service.Query,
        Air.Service.License,
        Air.Service.Central,
        Air.Service.AdminGuard,
        Air.Service.Export,
        Air.Service.Cleanup,
        Air.Service.RevokableToken,
        Air.Service.LDAP,
        in_env(test: nil, else: Air.Service.LDAP.PeriodicSync),
        Air.TimestampUpdater,
        Air.PsqlServer,
        Air.Web,
        in_env(test: nil, else: Air.Service.Explorer),
        AirWeb.MonitoringEndpoint,
        AirWeb.Telemetry,
        Air.BOM,
        Air.Service.User.LoginStats
      ]
      |> Enum.reject(&is_nil/1),
      strategy: :one_for_one,
      name: Air.Supervisor
    )
  end
end
