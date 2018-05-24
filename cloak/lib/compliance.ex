defmodule Compliance do
  @moduledoc "Compliance common functions"

  require Aircloak.DeployConfig

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates compliance database and populated tables for each datasource in the given configuration."
  @spec initialize(String.t(), pos_integer, pos_integer) :: :ok
  def initialize(config_name, num_users, concurrency) do
    config_name
    |> Compliance.DataSources.all_from_config()
    |> Compliance.DataSources.setup(
      Compliance.Data.users(num_users),
      num_users,
      concurrency
    )
  end

  @doc "Creates a complete compliance configuration for each datasource in the given configuration."
  @spec regenerate_config_from_db(String.t()) :: :ok
  def regenerate_config_from_db(config_name) do
    config_name
    |> Compliance.DataSources.all_from_config()
    |> Compliance.DataSources.complete_data_source_definitions()
    |> Enum.each(&store_json_config/1)
  end

  @doc "Stores the given datasource configuration to disk."
  @spec store_json_config(Cloak.DataSource.t()) :: :ok
  def store_json_config(data_source) do
    [
      Aircloak.File.config_dir_path(:cloak),
      Aircloak.DeployConfig.fetch!("data_sources"),
      "#{data_source.name}.json"
    ]
    |> Path.join()
    |> File.write!(json_config(data_source))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp json_config(data_source) do
    data_source
    |> Map.take([:name, :parameters])
    |> Map.put(:driver, "postgresql")
    |> Map.put(:tables, tables_map(data_source.tables))
    |> Poison.encode!(pretty: true)
  end

  defp tables_map(tables) do
    tables
    |> Enum.map(fn {name, table} -> {name, table_data(table)} end)
    |> Enum.into(%{})
  end

  defp table_data(table) do
    table
    |> Map.take([:db_name, :user_id, :projection, :decoders])
    |> Enum.reject(fn {_key, val} -> is_nil(val) end)
    |> Enum.into(%{})
  end
end
