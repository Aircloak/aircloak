defmodule AircloakCI.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    Supervisor.start_link(
      [
        AircloakCI.CmdRunner.Supervisor,
        AircloakCI.Github,
        AircloakCI.PullRequestProvider,
        AircloakCI.Builder.Server,
      ],
      strategy: :one_for_one,
      name: AircloakCI.Supervisor
    )
  end
end
