defmodule AircloakCI.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    Supervisor.start_link(
      [
        AircloakCI.CmdRunner.Supervisor,
        AircloakCI.Github,
        AircloakCI.RepoDataProvider,
        AircloakCI.Builder.Server,
        AircloakCI.BuildCleaner
      ],
      strategy: :one_for_one,
      name: AircloakCI.Supervisor
    )
  end
end
