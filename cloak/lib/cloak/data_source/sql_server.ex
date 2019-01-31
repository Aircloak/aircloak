defmodule Cloak.DataSource.SQLServer do
  @moduledoc "Implements the DataSource.Driver behaviour for MS SQL Server. For more information, see `DataSource`."

  alias Cloak.DataSource.RODBC

  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters) do
    with {:ok, connection} <- RODBC.connect(parameters, &conn_params/1) do
      :ok = RODBC.Port.execute(connection, "SET ANSI_DEFAULTS ON")
      {:ok, connection}
    end
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
  def sql_dialect_module(), do: SqlBuilder.SQLServer

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
      DSN: "SQLServer",
      Server: normalized_parameters[:hostname],
      Uid: normalized_parameters[:username],
      Pwd: normalized_parameters[:password],
      Database: normalized_parameters[:database]
    }
  end
end
