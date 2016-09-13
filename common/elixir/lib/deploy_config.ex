defmodule Aircloak.DeployConfig do
  @moduledoc """
  Loads the deploy-specific application parameters.

  This module exposes macros which can be used to fetch deploy-specific application
  parameters. To use the module, you need to `require Aircloak.DeployConfig`. Then
  you can invoke provided macros, which will fetch configuration for the current
  application (the one where the macro is invoked), and the current build environment.

  The parameters are fetched from files which are residing in the `priv/config`
  folder of the caller app. For development and test environments, files `dev.json` and
  `test.json` are used. For production, the file `config.json` is used.

  You shouldn't keep `config.json` in the repository, because it is specific for
  each deployment. In contrast, `dev.json` and `test.json` should be committed to
  have out-of-the-box default configurations for developers.
  """


  # -------------------------------------------------------------------
  # API macros
  # -------------------------------------------------------------------

  @doc "Retrieves a deploy-specific configuration value, raises if it's not found."
  defmacro fetch!(key), do:
    quote(do: Map.fetch!(unquote(__MODULE__).read_config!(), unquote(key)))

  @doc "Retrieves a deploy-specific configuration value."
  defmacro fetch(key), do:
    quote(do: Map.fetch(unquote(__MODULE__).read_config!(), unquote(key)))

  @doc "Retrieves a deploy-specific configuration value, returns a default value if not found."
  defmacro get(key, default \\ nil), do:
    quote(do: Map.get(unquote(__MODULE__).read_config!(), unquote(key), unquote(default)))


  # -------------------------------------------------------------------
  # Internal macros and functions
  # -------------------------------------------------------------------

  @doc false
  defmacro read_config!(), do:
    quote(do: unquote(__MODULE__).do_read_config(unquote(Mix.Project.config[:app]), unquote(Mix.env)))

  @doc false
  def do_read_config(app, env) do
    app_priv_dir = Application.app_dir(app, "priv")

    data_sources_file_name =
      case env do
        :dev -> "dev.json"
        :test -> "test.json"
        :prod -> "config.json"
      end

    Path.join([app_priv_dir, "config", data_sources_file_name])
    |> File.read!()
    |> Poison.decode!()
  end
end
