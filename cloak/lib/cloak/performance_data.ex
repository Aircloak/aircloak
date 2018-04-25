defmodule Cloak.PerformanceData do
  @moduledoc "Module for generating performance data and datasources."
  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Generate the performance data and datasource configuration files."
  @spec generate(
          hostname: String,
          port: pos_integer,
          username: String.t(),
          num_users: pos_integer
        ) :: :ok
  def generate(opts \\ []) do
    opts =
      Map.merge(
        %{hostname: "localhost", port: 5432, username: "cloak", num_users: 10_000},
        Map.new(opts)
      )

    [performance_config(opts)]
    |> Cloak.DataSource.config_to_datasources()
    |> Compliance.DataSources.setup(
      Compliance.Data.users(opts.num_users),
      opts.num_users,
      System.schedulers_online()
    )

    [performance_config(opts)]
    |> Cloak.DataSource.config_to_datasources()
    |> Compliance.DataSources.complete_data_source_definitions()
    |> Enum.each(&Compliance.store_json_config/1)
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
        "port" => opts.port
      },
      "tables" => []
    }
  end
end
