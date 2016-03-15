defmodule Air do
  @moduledoc false
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    configure_secrets()
    Air.Repo.configure()
    Air.Supervisor.start_link()
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Air.Endpoint.config_change(changed, removed)
    :ok
  end

  defp configure_secrets do
    Air.Utils.update_app_env(:guardian, Guardian,
        &[{:secret_key, :air_etcd.get("/settings/air/insights/secrets/guardian_key")} | &1])

    Air.Utils.update_app_env(:air, Air.Endpoint,
        &[{:secret_key_base, :air_etcd.get("/settings/air/insights/secrets/endpoint_key_base")} | &1])
  end
end
