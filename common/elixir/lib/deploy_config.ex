defmodule Aircloak.DeployConfig do
  @moduledoc """
  Loads the deploy-specific application parameters.

  This module exposes macros which can be used to fetch deploy-specific application
  parameters. To use the module, you need to `require Aircloak.DeployConfig`. Then
  you can invoke provided macros, which will fetch configuration for the current
  application (the one where the macro is invoked).

  The parameters are fetched from files which are residing in the `priv/config`
  folder of the caller app. The name of the file is obtained from the
  `:deploy_config_file` configuration of the application. In test environment on
  travis the file `travis.json` is used. For integration tests on travis, the file
  `integration_tests.json` is used.

  You shouldn't keep production config file in the repository, because it is
  specific for each deployment. In contrast, test and development configurations
  should be committed to have out-of-the-box default configurations for developers.
  """


  # -------------------------------------------------------------------
  # API macros and functions
  # -------------------------------------------------------------------

  @doc "Retrieves a deploy-specific configuration value of the calling app, raises if it's not found."
  defmacro fetch!(key), do:
    quote(do: unquote(__MODULE__).fetch!(unquote(Mix.Project.config[:app]), unquote(key)))

  @doc "Retrieves a deploy-specific configuration value of the calling app."
  defmacro fetch(key), do:
    quote(do: unquote(__MODULE__).fetch(unquote(Mix.Project.config[:app]), unquote(key)))

  @doc "Retrieves a deploy-specific configuration value of the given app, raises if it's not found."
  @spec fetch!(atom, any) :: any
  def fetch!(app, key), do:
    app |> read_config!() |> Map.fetch!(key)

  @doc "Retrieves a deploy-specific configuration value of the given app."
  @spec fetch(atom, any) :: {:ok, any} | :error
  def fetch(app, key), do:
    app |> read_config!() |> Map.fetch(key)


  @doc "Updates cached configuration value of the given app."
  @spec update(atom, any, (any -> any)) :: :ok
  def update(app, key, fun), do:
    Application.put_env(app, __MODULE__, Map.update!(read_config!(app), key, fun))


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp read_config!(app) do
    case Application.fetch_env(app, __MODULE__) do
      {:ok, config} ->
        config

      :error ->
        config = read_config_from_file(app)
        Application.put_env(app, __MODULE__, config)
        config
    end
  end

  defp read_config_from_file(app), do:
    config_file(app)
    |> File.read!()
    |> Poison.decode!()

  defp config_file(app), do:
    Path.join([Application.app_dir(app, "priv"), "config", config_file_name(app)])

  defp config_file_name(app) do
    case {
      Application.fetch_env!(:aircloak_common, :env),
      System.get_env("TRAVIS"),
      System.get_env("INTEGRATION_TEST")
    } do
      {:test, "true", "true"} -> "integration_tests.json"
      {:test, "true", _} -> "travis.json"
      _ -> Application.fetch_env!(app, :deploy_config_file)
    end
  end
end
