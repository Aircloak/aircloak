defmodule Air.Supervisor do
  @moduledoc false

  def start_link,
    do:
      Supervisor.start_link(
        common_processes() ++ system_processes(),
        strategy: :one_for_one,
        name: Air.Supervisor
      )

  defp common_processes(),
    do: [
      Air.Repo,
      Air.Repo.Migrator,
      Air.Service.Cloak,
      Air.Service.DataSource,
      Air.Service.View,
      Air.Service.Query,
      Air.Service.License,
      Air.Service.Central,
      Air.Service.User,
      Air.Service.Export,
      Air.ApiTokenTimestampUpdater,
      AirWeb.Endpoint,
      AirWeb.MonitoringEndpoint,
      Air.BOM,
      Air.PsqlServer,
      Air.PsqlServer.ConnectionRegistry
    ]

  if Mix.env() == :test do
    defp system_processes(), do: []
  else
    defp system_processes(),
      do: [
        Air.Scheduler
      ]
  end
end
