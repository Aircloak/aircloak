defmodule Air do
  @moduledoc "Air application behaviour and some common helper functions."
  use Application
  require Logger
  require Aircloak.DeployConfig
  require Aircloak.File

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns the site setting from the current deployment configuration (config.json)
  and raises if the parameter isn't found.
  """
  @spec site_setting!(any) :: any
  def site_setting!(name) do
    {:ok, value} = site_setting(name)
    value
  end

  @doc "Returns the site setting from the current deployment configuration (config.json)."
  @spec site_setting(any) :: {:ok, any} | :error
  def site_setting(name),
    do: with({:ok, site_config} <- Aircloak.DeployConfig.fetch("site"), do: Map.fetch(site_config, name))

  @doc "Returns the name of this air instance"
  @spec instance_name() :: String.t()
  def instance_name() do
    vm_short_name =
      Node.self()
      |> Atom.to_string()
      |> String.split("@")
      |> hd()

    {:ok, hostname} = :inet.gethostname()

    "#{vm_short_name}@#{hostname}"
  end

  # -------------------------------------------------------------------
  # Application behaviour functions
  # -------------------------------------------------------------------

  @impl Application
  def start(_type, _args) do
    Aircloak.DeployConfig.validate!(:air)
    configure_secrets()
    configure_appsignal()
    Air.Repo.configure()
    Air.PsqlServer.ShadowDb.init_queue()

    with {:ok, _pid} = result <- Air.Supervisor.start_link() do
      maybe_load_license()
      maybe_load_privacy_policy()
      result
    end
  end

  @doc false
  def config_change(changed, _new, removed) do
    AirWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp configure_secrets do
    Air.Utils.update_app_env(
      :air,
      Air.Guardian,
      &[{:secret_key, site_setting!("auth_secret")} | &1]
    )

    Air.Utils.update_app_env(
      :air,
      AirWeb.Endpoint,
      &Keyword.merge(
        &1,
        secret_key_base: site_setting!("endpoint_key_base"),
        https: https_config(Keyword.get(&1, :https, []))
      )
    )
  end

  defp configure_appsignal() do
    case Aircloak.DeployConfig.fetch("appsignal") do
      :error ->
        :ignore

      {:ok, config} ->
        config =
          config
          |> Aircloak.atomize_keys()
          |> Map.put_new(:active, true)
          |> Map.put_new(:skip_session_data, true)
          |> Map.put_new(:filter_parameters, ["password", "token", "secret"])

        Air.Utils.update_app_env(:appsignal, :config, fn _ -> config end)
        Appsignal.config_change(:ignored, :ignored, :ignored)
    end
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
            Keyword.merge(
              previous_https_config,
              port: Application.fetch_env!(:air, :https_port),
              keyfile: keyfile_path,
              certfile: certfile_path
            )

          {false, _} ->
            Logger.warn("the file `#{keyfile}` is missing")
            nil

          {_, false} ->
            Logger.warn("the file `#{certfile}` is missing")
            nil
        end
    end
  end

  # -------------------------------------------------------------------
  # Post boot static configuration
  # -------------------------------------------------------------------

  defp maybe_load_license() do
    on_setting_file("license_file", fn license_content, path ->
      case Air.Service.License.load(license_content) do
        :ok -> Logger.info("Applied statically configured Aircloak license from file `#{path}`")
        {:error, reason} -> Logger.error("Failed to load an Aircloak Insights license from file `#{path}`: " <> reason)
      end
    end)
  end

  defp maybe_load_privacy_policy() do
    on_setting_file("privacy_policy_file", fn policy_content, path ->
      if Air.Service.PrivacyPolicy.exists?() do
        {:ok, current_policy} = Air.Service.PrivacyPolicy.get()

        unless current_policy.content == policy_content do
          Logger.error(
            "The system is statically configured with a privacy policy, but has already been given " <>
              "a different one. The statically configured privacy policy will not be applied."
          )
        end
      else
        Air.Service.PrivacyPolicy.set(policy_content)
        Logger.info("Applied statically configured privacy policy from file `#{path}`")
      end
    end)
  end

  defp on_setting_file(setting, callback) do
    case site_setting(setting) do
      {:ok, setting_file_path} ->
        case Aircloak.File.read(setting_file_path) do
          {:error, reason} ->
            Logger.error(
              "Could not read file `#{setting_file_path}` configured under site configuration parameter " <>
                "`#{setting}`. The reported error is: " <> Aircloak.File.humanize_posix_error(reason)
            )

          setting_content ->
            callback.(setting_content, setting_file_path)
        end

      :error ->
        :ok
    end
  end
end
