defmodule Central do
  @moduledoc false
  use Application
  require Aircloak.DeployConfig

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    configure_secrets()
    Central.Repo.configure()
    Central.Supervisor.start_link()
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    CentralWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  # returns the site setting from the current deployment configuration (config.json)
  def site_setting(name), do: Map.fetch!(Aircloak.DeployConfig.fetch!("site"), name)

  defp configure_secrets do
    Central.Utils.update_app_env(:guardian, Guardian,
      &[{:secret_key, site_setting("auth_secret")} | &1])

    Central.Utils.update_app_env(:central, CentralWeb.Endpoint, fn(config) ->
      [
        {:secret_key_base, site_setting("endpoint_key_base")},
        {:customer_token_salt, site_setting("customer_token_salt")}
        | config
      ] ++ https_config()
    end)
  end

  defp https_config() do
    keyfile = Path.join([Application.app_dir(:central, "priv"), "config", "ssl_key.pem"])
    certfile = Path.join([Application.app_dir(:central, "priv"), "config", "ssl_cert.pem"])

    if File.exists?(keyfile) && File.exists?(certfile) do
      [https: [port: Application.fetch_env!(:central, :https_port), keyfile: keyfile, certfile: certfile]]
    else
      []
    end
  end
end
