defmodule Cloak.DataSource.SAPHana do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP HANA using the Rust ODBC port driver.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.RODBC

  use Cloak.DataSource.Driver.SQL

  @doc """
  Returns the SAP HANA schema as configured in app config.
  This is useful in development, to allow different developers to work on different schemas.
  """
  @spec default_schema() :: nil | String.t()
  def default_schema() do
    non_empty_schema(System.get_env("__AC__DEFAULT_SAP_HANA_SCHEMA__")) ||
      non_empty_schema(default_schema_from_app_config())
  end

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(), do: Cloak.DataSource.SqlBuilder.SAPHana

  @impl Driver
  def connect(parameters) do
    if File.exists?(Cloak.SapHanaHelpers.driver_path()),
      do: RODBC.connect(parameters, &conn_params/1),
      else: {:error, "ODBC driver for SAP HANA is not mounted."}
  end

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table),
    do: RODBC.load_tables(connection, update_in(table.db_name, &SqlBuilder.quote_table_name/1))

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: RODBC

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  # -------------------------------------------------------------------
  # DataSource.Driver.SQL callbacks
  # -------------------------------------------------------------------

  @impl Driver.SQL
  def execute(connection, sql), do: RODBC.execute_direct(connection, sql)

  @impl Driver.SQL
  def select(connection, sql), do: execute(connection, sql)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    %{
      servernode: "#{normalized_parameters[:hostname]}:#{normalized_parameters[:port]}",
      Uid: normalized_parameters[:username],
      Pwd: normalized_parameters[:password],
      databasename: normalized_parameters[:database],
      DSN: "SAPHana"
    }
    |> Map.merge(schema_option(Cloak.DataSource.SAPHana.default_schema()))
  end

  defp schema_option(nil), do: %{}
  defp schema_option(schema), do: %{cs: ~s/"#{schema}"/}

  defp default_schema_from_app_config() do
    with {:ok, saphana_settings} <- Application.fetch_env(:cloak, :sap_hana),
         {:ok, default_schema} <- Keyword.fetch(saphana_settings, :default_schema) do
      default_schema
    else
      _ -> nil
    end
  end

  defp non_empty_schema(nil), do: nil
  defp non_empty_schema(""), do: nil
  defp non_empty_schema(schema) when is_binary(schema), do: schema
end
