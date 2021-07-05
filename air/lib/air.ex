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

  @doc "Returns the configured name of the aircloak instance."
  @spec name() :: String.t()
  def name(), do: Aircloak.DeployConfig.fetch!("name")

  @doc "Returns the socket transport for browser clients."
  @spec browser_socket_transport() :: :long_polling | :websocket
  def browser_socket_transport() do
    case site_setting("browser_long_polling") do
      {:ok, true} -> :long_polling
      {:ok, false} -> :websocket
      :error -> :websocket
    end
  end

  # -------------------------------------------------------------------
  # Application behaviour functions
  # -------------------------------------------------------------------

  @impl Application
  def start(_type, _args) do
    Aircloak.DeployConfig.validate!(:air)
    configure_secrets()
    Air.Repo.configure()

    with {:ok, _pid} = result <- Air.Supervisor.start_link() do
      start_log_collection()
      load_privacy_policy()
      load_users_and_datasources()
      prepare_explorer()
      log_startup()
      result
    end
  end

  @doc false
  @impl Application
  def config_change(changed, _new, removed) do
    AirWeb.Endpoint.config_change(changed, removed)
    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp log_startup() do
    version = Aircloak.Version.for_app(:air)
    Logger.info("Insights Air version #{version} started [name: '#{name()}', instance: '#{instance_name()}']")
  end

  defp prepare_explorer() do
    Air.Service.Explorer.setup_credentials_if_required()
  end

  defp configure_secrets do
    Air.Utils.update_app_env(
      :air,
      AirWeb.Endpoint,
      fn env ->
        env
        |> Keyword.merge(
          secret_key_base: site_setting!("endpoint_key_base"),
          https: https_config(Keyword.get(env, :https, []))
        )
        |> conditionally_add_endpoint_url()
      end
    )
  end

  defp conditionally_add_endpoint_url(current_env) do
    with {:ok, url} <- site_setting("endpoint_public_url"),
         %URI{host: host, path: path, port: port, scheme: scheme} <-
           URI.parse(url) do
      Keyword.merge(
        current_env,
        url: [host: host, path: path, port: port, scheme: scheme]
      )
    else
      _ -> current_env
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

  defp load_privacy_policy() do
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

  defp load_users_and_datasources() do
    on_setting_file("users_and_datasources_file", fn raw_users_and_datasources, path ->
      case Aircloak.Json.permissive_decode(raw_users_and_datasources) do
        {:ok, map} ->
          main_error =
            "Could not validate the format of the users and data sources file `#{path}` " <>
              "configured in the Insights Air configuration under the site parameter"

          case Aircloak.validate_decoded_json(:air, "users_and_datasources_schema.json", map, main_error) do
            :ok ->
              map = Aircloak.atomize_keys(map)
              add_users(map.users)
              add_data_sources(map.data_sources)

            {:error, message} ->
              Logger.error(message)
          end

        {:error, message} ->
          Logger.error("Failed at parsing users and datasource file at path `#{path}`: #{message}")
      end
    end)
  end

  defp add_users(users) do
    users
    |> Enum.map(fn user_data ->
      case Air.Service.User.add_preconfigured_user(user_data) do
        :error -> nil
        {:ok, user} -> user
      end
    end)
    |> Enum.reject(&is_nil/1)
    |> Enum.flat_map(&Air.Service.User.logins(&1))
    |> case do
      [] -> :ok
      added_logins -> Logger.info("Added user accounts for logins: #{Enum.join(added_logins, ", ")}")
    end
  end

  def add_data_sources(data_sources) do
    data_sources
    |> Enum.each(fn %{name: name, group_name: group_name} = data_source_data ->
      case Air.Service.DataSource.add_preconfigured_datasource(data_source_data) do
        {:error, :no_users} ->
          Logger.info("Did not add data source `#{name}`. It would not be available to any registered users.")

        {:error, :group_exists} ->
          Logger.info("Did not add data source `#{name}`. A group with name `#{group_name}` already exists.")

        {:error, :data_source_exists} ->
          Logger.info("Did not add data source `#{name}`. It already exists.")

        {:ok, data_source} ->
          user_logins =
            data_source
            |> Air.Service.DataSource.users()
            |> Enum.flat_map(&Air.Service.User.logins(&1))
            |> Enum.join(", ")

          Logger.info("Added data source `#{name}`. It's available to the following users: #{user_logins}.")
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

  if Mix.env() == :test do
    defp start_log_collection(), do: :ok
  else
    defp start_log_collection(), do: Logger.add_backend(Air.Service.Logs.Collector, flush: true)
  end
end
