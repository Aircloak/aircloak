defmodule Air do
  @moduledoc "Air application behaviour and some common helper functions."
  use Application
  require Logger
  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the site setting from the current deployment configuration (config.json)."
  @spec site_setting(any) :: any
  def site_setting(name), do: Map.fetch!(Aircloak.DeployConfig.fetch!("site"), name)

  @doc "Returns the name of this air instance"
  @spec instance_name() :: String.t
  def instance_name() do
    vm_short_name =
      Node.self()
      |> Atom.to_string()
      |> String.split("@")
      |> hd()
    {:ok, hostname} = :inet.gethostname()

    "#{vm_short_name}@#{hostname}"
  end

  @doc "Returns the customer token"
  @spec customer_token() :: String.t
  def customer_token(), do: site_setting("customer_token")


  # -------------------------------------------------------------------
  # Application behaviour functions
  # -------------------------------------------------------------------

  @doc false
  def start(_type, _args) do
    configure_secrets()
    Air.Repo.configure()
    configure_periodic_jobs()
    Air.Supervisor.start_link()
  end

  @doc false
  def config_change(changed, _new, removed) do
    Air.Endpoint.config_change(changed, removed)
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp configure_secrets do
    Air.Utils.update_app_env(:guardian, Guardian,
      &[{:secret_key, site_setting("auth_secret")} | &1])

    Air.Utils.update_app_env(:air, Air.Endpoint,
      &Keyword.merge(&1,
        secret_key_base: site_setting("endpoint_key_base"),
        api_token_salt: site_setting("api_token_salt"),
        https: https_config(Keyword.get(&1, :https, []))
      )
    )
  end

  defp https_config(previous_https_config) do
    case {
      Map.get(Aircloak.DeployConfig.fetch!("site"), "keyfile"),
      Map.get(Aircloak.DeployConfig.fetch!("site"), "certfile")
    } do
      {nil, nil} ->
        nil

      {nil, _} ->
        Logger.warn("`keyfile` is not specified under the `site` key in config.json")
        nil

      {_, nil} ->
        Logger.warn("`certfile` is not specified under the `site` key in config.json")
        nil

      {keyfile, certfile} ->
        keyfile_path = Path.join([Application.app_dir(:air, "priv"), "config", keyfile])
        certfile_path = Path.join([Application.app_dir(:air, "priv"), "config", certfile])

        case {File.exists?(keyfile_path), File.exists?(certfile_path)} do
          {true, true} ->
            Keyword.merge(previous_https_config,
              port: Application.fetch_env!(:air, :https_port), keyfile: keyfile_path, certfile: certfile_path)
          {false, _} ->
            Logger.warn("the file `#{keyfile}` is missing")
            nil
          {_, false} ->
            Logger.warn("the file `#{certfile}` is missing")
            nil
        end
    end
  end

  if Mix.env == :test do
    defp configure_periodic_jobs(), do: :ok
  else
    defp configure_periodic_jobs() do
      import Crontab.CronExpression

      [
        {"0 * * * *", {Air.Service.Cleanup, :cleanup_old_queries}},
        {"*/5 * * * *", {Air.Service.Cleanup, :cleanup_dead_queries}},
        {~e[*/10 * * * * * *]e, {Air.Socket.Frontend.DataSourceChannel, :push_updates}},
      ]
      |> Enum.each(fn({schedule, job}) -> Quantum.add_job(schedule, job) end)
    end
  end
end
