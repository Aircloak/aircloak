defmodule AircloakCI.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    Supervisor.start_link(
      processes(),
      strategy: :one_for_one,
      name: AircloakCI.Supervisor
    )
  end

  if Mix.env == :prod do
    defp processes(), do: common_processes() ++ [AircloakCI.Github.StatusPoller]
  else
    defp processes(), do: common_processes()
  end

  defp common_processes(), do:
    [
      AircloakCI.CmdRunner.Supervisor,
      AircloakCI.Github,
      AircloakCI.Builder.Server,
    ]
end
