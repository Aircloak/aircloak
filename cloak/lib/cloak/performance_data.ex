defmodule Cloak.PerformanceData do
  @moduledoc "Module for generating performance data and datasources."
  require Aircloak.DeployConfig


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generate the performance data and datasource configuration files."
  @spec generate([hostname: String, port: pos_integer, username: String.t, num_users: pos_integer]) :: :ok
  def generate(opts \\ []) do
    opts =
      Map.merge(
        %{hostname: "localhost", port: 5432, username: "cloak", num_users: 10_000},
        Map.new(opts)
      )

    [performance_config(opts)]
    |> Cloak.DataSource.config_to_datasources()
    |> Compliance.DataSources.setup(Compliance.Data.users(opts.num_users), opts.num_users, System.schedulers_online())

    [performance_config(opts)]
    |> Cloak.DataSource.config_to_datasources()
    |> Compliance.DataSources.complete_data_source_definitions()
    |> Enum.each(&store_json_config/1)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp performance_config(opts) do
    %{
      "driver" => "postgresql",
      "name" => "cloak_performance",
      "parameters" => %{
        "database" => "cloak_performance",
        "hostname" => opts.hostname,
        "username" => opts.username,
        "port" => opts.port,
      },
      "tables" => []
    }
  end

  defp store_json_config(data_source) do
    [
      Aircloak.File.config_dir_path(:cloak),
      Aircloak.DeployConfig.fetch!("data_sources"),
      "#{data_source.name}.json"
    ]
    |> Path.join()
    |> File.write!(json_config(data_source))
  end

  defp json_config(data_source) do
    data_source
    |> Map.take([:name, :parameters])
    |> Map.put(:driver, "postgresql")
    |> Map.put(:tables, tables_map(data_source.tables))
    |> Poison.encode!(pretty: true)
  end

  defp tables_map(tables) do
    tables
    |> Enum.map(fn({name, table}) -> {name, Map.take(table, [:db_name, :user_id, :projection, :decoders])} end)
    |> Enum.into(%{})
  end
end
