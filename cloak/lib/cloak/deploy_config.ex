defmodule Cloak.DeployConfig do
  @moduledoc """
  Loads the deploy-specific configuration.

  This module is used to load the deploy-specific configuration parameters. The
  parameters are specified in files which are residing in the `priv/config`
  folder of this app. For development and test environments, files `dev.json` and
  `test.json` are used. For production, the file `config.json` is used.

  Dev/test configurations are committed in this repository. The production
  configuration needs to be created by the users, since no sensible defaults
  can be provided.
  """


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Retrieves a deploy-specific configuration value, raises if it's not found."
  @spec fetch!(String.t) :: any
  def fetch!(key) do
    {:ok, value} = fetch(key)
    value
  end

  @doc "Retrieves a deploy-specific configuration value."
  @spec fetch(String.t) :: {:ok, any} | :error
  def fetch(key) do
    read_config!()
    |> Map.fetch(key)
  end

  @doc "Retrieves a deploy-specific configuration value, returns a default value if not found."
  @spec get(String.t) :: any
  def get(key, default \\ nil) do
    read_config!()
    |> Map.get(key, default)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @data_sources_file_name (
    case Mix.env do
      :dev -> "dev.json"
      :test -> if System.get_env("TRAVIS") == "true "do "travis.json" else "test.json" end
      :prod -> "config.json"
    end
  )

  defp read_config!() do
    Path.join([Application.app_dir(:cloak, "priv"), "config", @data_sources_file_name])
    |> File.read!()
    |> Poison.decode!()
  end
end
