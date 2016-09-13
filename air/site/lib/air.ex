defmodule Air do
  @moduledoc false
  use Application
  require Aircloak.DeployConfig

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    configure_secrets()
    configure_endpoint_url()
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
      &[{:secret_key, site_setting("auth_secret")} | &1])

    Air.Utils.update_app_env(:air, Air.Endpoint, fn(config) ->
      [
        {:secret_key_base, site_setting("endpoint_key_base")},
        {:api_token_salt, site_setting("api_token_salt")},
        {:data_source_token_salt, site_setting("data_source_token_salt")}
        | config
      ]
    end)
  end

  # Configures the url setting of the Air.Endpoint config,
  # allowing phoenix to reject connections under unexpected urls.
  defp configure_endpoint_url do
    url_config = [host: site_setting("url")]
    Air.Utils.update_app_env(:air, Air.Endpoint, &Keyword.merge(&1, url: url_config))
  end

  defp site_setting(name), do: Map.fetch!(Aircloak.DeployConfig.fetch!("site"), name)
end
