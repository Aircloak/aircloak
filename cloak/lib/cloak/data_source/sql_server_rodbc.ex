defmodule Cloak.DataSource.SQLServerRODBC do
  @moduledoc "Implements the DataSource.Driver behaviour for MS SQL Server. For more information, see `DataSource`."

  alias Cloak.DataSource.RODBC

  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect!(parameters) do
    connection = RODBC.connect!(parameters, &conn_params/1)
    :ok = RODBC.Port.execute(connection, "SET ANSI_DEFAULTS ON")
    connection
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

  @impl Driver
  def sql_dialect_module(_parameters), do: SqlBuilder.SQLServer

  @impl Driver
  defdelegate cast_to_text?(), to: RODBC

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    %{
      DSN: "SQLServer",
      Server: normalized_parameters[:hostname],
      Uid: normalized_parameters[:username],
      Pwd: normalized_parameters[:password],
      Database: normalized_parameters[:database]
    }
  end
end
