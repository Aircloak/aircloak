defmodule Cloak.DataSource.DrillRODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Apache Drill using the Rust ODBC port driver.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  alias Cloak.DataSource
  alias Cloak.DataSource.RODBC

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_), do: Cloak.DataSource.SqlBuilder.Drill

  @impl Driver
  def connect!(parameters), do: RODBC.connect!(parameters, &DataSource.Drill.conn_params/1)

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
end
