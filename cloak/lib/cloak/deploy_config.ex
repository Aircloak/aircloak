defmodule Cloak.DeployConfig do
  @moduledoc """
  Loads the deploy-specific configuration.

  This module is used to load the deploy-specific configuration parameters and
  merge them into the application environment.

  The parameters are specified in files which are residing in the `priv/config`
  folder of this app. For development and test environments, files `dev.json` and
  `test.json` are used. For production, the file `config.json` is used.

  Dev/test configurations are committed in this repository. The production
  configuration needs to be created by the users, since no sensible defaults
  can be provided.
  """


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Loads the deploy-specific configuration."
  @spec load() :: :ok
  def load() do
    read_config!()
    |> Enum.each(&handle_config/1)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @data_sources_file_name (
    case Mix.env do
      :dev -> "dev.json"
      :test -> "test.json"
      :prod -> "config.json"
    end
  )

  defp read_config!() do
    Path.join([Application.app_dir(:cloak, "priv"), "config", @data_sources_file_name])
    |> File.read!()
    |> Poison.decode!()
  end

  defp handle_config({"air_socket_url", socket_url}) do
    Application.put_env(:cloak, :air,
      Keyword.put(Application.get_env(:cloak, :air), :socket_url, socket_url)
    )
  end
  defp handle_config({"data_sources", data_sources}) do
    Application.put_env(:cloak, :data_sources,
      data_sources
      |> atomize_keys()
      |> map_drivers()
    )
  end

  defp map_drivers(data_sources) do
    Enum.map(data_sources, fn({data_source, params}) ->
      driver_module = case params[:driver] do
        "postgresql" -> Cloak.DataSource.PostgreSQL
        other -> raise("Unknown driver `#{other}` for data source `#{data_source}`")
      end

      {data_source, Map.put(params, :driver, driver_module)}
    end)
  end

  defp atomize_keys(%{} = map) do
    for {key, value} <- map, into: %{}, do: {String.to_atom(key), atomize_keys(value)}
  end
  defp atomize_keys(list) when is_list(list) do
    Enum.map(list, &atomize_keys/1)
  end
  defp atomize_keys(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> atomize_keys()
    |> List.to_tuple()
  end
  defp atomize_keys(other), do: other
end
