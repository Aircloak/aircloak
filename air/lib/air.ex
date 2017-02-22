defmodule Air do
  @moduledoc false
  use Application
  require Aircloak.DeployConfig

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

  # returns the site setting from the current deployment configuration (config.json)
  def site_setting(name), do: Map.fetch!(Aircloak.DeployConfig.fetch!("site"), name)

  def instance_name() do
    vm_short_name =
      Node.self()
      |> Atom.to_string()
      |> String.split("@")
      |> hd()
    {:ok, hostname} = :inet.gethostname()

    "#{vm_short_name}@#{hostname}"
  end

  def customer_token(), do: site_setting("customer_token")


  defp configure_secrets do
    Air.Utils.update_app_env(:guardian, Guardian,
      &[{:secret_key, site_setting("auth_secret")} | &1])

    Air.Utils.update_app_env(:air, Air.Endpoint, fn(config) ->
      [
        {:secret_key_base, site_setting("endpoint_key_base")},
        {:api_token_salt, site_setting("api_token_salt")}
        | config
      ] ++ https_config(Keyword.get(config, :https, []))
    end)
  end

  defp https_config(previous_https_config) do
    keyfile = Path.join([Application.app_dir(:air, "priv"), "config", "ssl_key.pem"])
    certfile = Path.join([Application.app_dir(:air, "priv"), "config", "ssl_cert.pem"])

    if File.exists?(keyfile) && File.exists?(certfile) do
      [https: Keyword.merge(previous_https_config,
        port: Application.fetch_env!(:air, :https_port), keyfile: keyfile, certfile: certfile)]
    else
      []
    end
  end
end
