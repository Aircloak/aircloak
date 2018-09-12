defmodule Cloak.DataSource.SAPHanaRODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP HANA using the Rust ODBC port driver.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.RODBC
  alias Cloak.DataSource

  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_), do: Cloak.DataSource.SqlBuilder.SAPHana

  @impl Driver
  def connect!(parameters) do
    unless File.exists?(Cloak.SapHanaHelpers.driver_path()),
      do: DataSource.raise_error("ODBC driver for SAP HANA is not mounted.")

    RODBC.connect!(parameters, &conn_params/1)
  end

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  defdelegate load_tables(connection, table), to: RODBC

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: RODBC

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: RODBC

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
end
