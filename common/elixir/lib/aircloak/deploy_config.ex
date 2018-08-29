defmodule Aircloak.DeployConfig do
  @moduledoc """
  Loads the deploy-specific application parameters.

  This module exposes macros which can be used to fetch deploy-specific application
  parameters. To use the module, you need to `require Aircloak.DeployConfig`. Then
  you can invoke provided macros, which will fetch configuration for the current
  application (the one where the macro is invoked).

  The parameters are fetched from files which are residing in the `priv/config`
  folder of the caller app. The name of the file is obtained from the
  `DEPLOY_CONFIG` OS environment variable,or `:deploy_config_file` configuration of
  the application.

  You shouldn't keep production config file in the repository, because it is
  specific for each deployment. In contrast, test and development configurations
  should be committed to have out-of-the-box default configurations for developers.
  """

  # -------------------------------------------------------------------
  # API macros and functions
  # -------------------------------------------------------------------

  @doc "Retrieves a deploy-specific configuration value of the calling app, raises if it's not found."
  defmacro fetch!(key), do: quote(do: unquote(__MODULE__).fetch!(unquote(Mix.Project.config()[:app]), unquote(key)))

  @doc "Retrieves a deploy-specific configuration value of the calling app."
  defmacro fetch(key), do: quote(do: unquote(__MODULE__).fetch(unquote(Mix.Project.config()[:app]), unquote(key)))

  @doc "Retrieves a deploy-specific configuration value of the calling app."
  defmacro get(key, default \\ nil) do
    quote do
      case unquote(__MODULE__).fetch(unquote(key)) do
        {:ok, value} -> value
        :error -> unquote(default)
      end
    end
  end

  @doc "Retrieves a deploy-specific configuration or the value from the calling app environment."
  defmacro override_app_env!(key),
    do: quote(do: unquote(__MODULE__).override_app_env!(unquote(Mix.Project.config()[:app]), unquote(key)))

  @doc "Retrieves a deploy-specific configuration value of the given app, raises if it's not found."
  @spec fetch!(atom, any) :: any
  def fetch!(app, key), do: app |> read_config!() |> Map.fetch!(key)

  @doc "Retrieves a deploy-specific configuration value of the given app."
  @spec fetch(atom, any) :: {:ok, any} | :error
  def fetch(app, key), do: app |> read_config!() |> Map.fetch(key)

  @doc "Retrieves a deploy-specific configuration or the value from the application environment."
  @spec override_app_env!(atom, atom) :: any
  def override_app_env!(app, key) do
    case fetch(app, to_string(key)) do
      {:ok, value} -> value
      :error -> Application.fetch_env!(app, key)
    end
  end

  @doc "Updates cached configuration value of the calling app."
  defmacro update(key, fun),
    do:
      quote(
        do:
          unquote(__MODULE__).update(
            unquote(Mix.Project.config()[:app]),
            unquote(key),
            unquote(fun)
          )
      )

  @doc "Updates cached configuration value of the given app."
  @spec update(atom, any, (any -> any)) :: :ok
  def update(app, key, fun), do: Application.put_env(app, __MODULE__, Map.update!(read_config!(app), key, fun))

  @doc "Validates the schema of the config.json according to priv/config_schema.json file."
  @spec validate!(atom, Map.t() | nil) :: :ok
  def validate!(app, config \\ nil) do
    config = config || read_config!(app)
    error_message = "configuration file is not valid"

    with {:error, error} <- Aircloak.validate_decoded_json(app, "config_schema.json", config, error_message),
         do: raise(error)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp read_config!(app) do
    case Application.fetch_env(app, __MODULE__) do
      {:ok, config} ->
        config

      :error ->
        {:ok, config} = Aircloak.File.read_config_file(app, config_file_name(app))
        Application.put_env(app, __MODULE__, config)
        config
    end
  end

  defp config_file_name(app) do
    cond do
      (config = System.get_env("DEPLOY_CONFIG")) != nil -> "#{config}.json"
      System.get_env("PERFORMANCE_TEST") == "true" -> "perf.json"
      true -> Application.fetch_env!(app, :deploy_config_file)
    end
  end
end
