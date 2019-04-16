defmodule Cloak.DataSource.SQLServer do
  @moduledoc "Implements the DataSource.Driver behaviour for MS SQL Server. For more information, see `DataSource`."

  alias Cloak.DataSource.RODBC
  use Cloak.DataSource.Driver.RodbcSql

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters) do
    with {:ok, connection} <- RODBC.connect(parameters, &conn_params/1) do
      :ok = RODBC.Port.execute(connection, "SET ANSI_DEFAULTS ON; SET ANSI_WARNINGS OFF; SET ARITHABORT OFF;")
      {:ok, connection}
    end
  end

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
