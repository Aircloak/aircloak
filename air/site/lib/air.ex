defmodule Air do
  @moduledoc false
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    Air.Repo.configure()
    Air.Supervisor.start_link()
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Air.Endpoint.config_change(changed, removed)
    :ok
  end
end
