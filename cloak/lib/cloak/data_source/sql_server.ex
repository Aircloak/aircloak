defmodule Cloak.DataSource.SQLServer do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Microsoft SQL Server.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.ODBC

  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect!(parameters) do
    connection = ODBC.connect!(parameters, &conn_params/1)
    {:updated, _} = :odbc.sql_query(connection, 'SET ANSI_DEFAULTS ON')
    connection
  end

  @impl Driver
  defdelegate disconnect(connection), to: ODBC

  @impl Driver
  defdelegate load_tables(connection, table), to: ODBC

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: ODBC

  @impl Driver
  defdelegate driver_info(connection), to: ODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: ODBC

  @impl Driver
  defdelegate cast_to_text?(), to: ODBC

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
