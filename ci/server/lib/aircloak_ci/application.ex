defmodule AircloakCI.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    Supervisor.start_link(
      [
        AircloakCI.CmdRunner.Supervisor,
        AircloakCI.Github.RateLimiter,
        AircloakCI.Builder.Server,
        AircloakCI.Github.StatusPoller,
      ],
      strategy: :one_for_one,
      name: AircloakCI.Supervisor
    )
  end
end
