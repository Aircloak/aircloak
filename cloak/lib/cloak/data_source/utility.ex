defmodule Cloak.DataSource.Utility do
  @moduledoc """
  This module contains utility functions that can be used when working with data sources.
  """

  require Logger
  require Aircloak.File
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc "This function will return a list of data source definitions loaded from all .json files in the given directory."
  @spec load_individual_data_source_configs(String.t()) :: [DataSource.t()]
  def load_individual_data_source_configs(config_path) when is_binary(config_path) do
    case Aircloak.File.ls(config_path) do
      {:ok, data_source_config_files} ->
        data_source_config_files
        |> Stream.filter(&(Path.extname(&1) == ".json"))
        |> Stream.map(fn file_name ->
          path = Path.join(config_path, file_name)

          case Aircloak.File.read_config_file(path) do
            {:ok, data_source_definition} ->
              validate_data_source(file_name, data_source_definition)

            {:error, :eacces} ->
              Logger.error(
                "Insights Cloak has insufficient file system privileges to read the " <>
                  "datasource config from `#{path}`"
              )

              nil

            {:error, reason} ->
              Logger.error("Failed at reading datasource config from `#{path}`: #{reason}")
              nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      {:error, reason} ->
        Logger.error(
          "Failed at loading data sources configurations from `#{config_path}`. " <>
            "Reason: #{Aircloak.File.humanize_posix_error(reason)}."
        )

        []
    end
  end

  @driver_name_to_module_mappings [
    {"postgresql", Cloak.DataSource.PostgreSQL},
    {"sqlserver", Cloak.DataSource.SQLServer},
    {"oracle", Cloak.DataSource.Oracle},
    {"cloudera-impala", Cloak.DataSource.ClouderaImpala}
  ]

  @doc "Returns the data source driver module given a data source type name"
  @spec name_to_driver(String.t()) :: {:ok, atom} | {:error, :unknown}
  Enum.each(@driver_name_to_module_mappings, fn {name, driver} ->
    def name_to_driver(unquote(name)), do: {:ok, unquote(driver)}
  end)

  def name_to_driver(_other), do: {:error, :unknown}

  @doc "Returns the data source type name given a driver module"
  @spec driver_to_name(atom) :: {:ok, String.t()} | {:error, :unknown}
  @driver_name_to_module_mappings
  |> Enum.each(fn {name, driver} ->
    def driver_to_name(unquote(driver)), do: {:ok, unquote(name)}
  end)

  def driver_to_name(_other), do: {:error, :unknown}

  @doc "Validates the data source schema for the loaded data source configuration."
  @spec validate_data_source(String.t(), Map.t()) :: DataSource.t() | nil
  def validate_data_source(data_source_name, data_source) do
    error_message = "invalid datasource configuration for datasource #{data_source_name}"

    case Aircloak.validate_decoded_json(:cloak, "datasource_schema.json", data_source, error_message) do
      :ok ->
        data_source

      {:error, error} ->
        Logger.error(error)
        nil
    end
  end
end
