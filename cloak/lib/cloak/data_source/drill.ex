defmodule Cloak.DataSource.Drill do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Apache Drill.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{ODBC, SqlBuilder}
  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_), do: SqlBuilder.Drill

  @impl Driver
  def connect!(parameters) do
    connection = ODBC.connect!(parameters, &SqlBuilder.Drill.conn_params/1)

    case :odbc.sql_query(connection, 'ALTER SESSION SET planner.parser.quoting_identifiers = \'"\'') do
      {:updated, _} -> :ok
      {:selected, ['ok', _], [[true, _]]} -> :ok
    end

    connection
  end

  @impl Driver
  defdelegate disconnect(connection), to: ODBC

  @impl Driver
  def load_tables(connection, table), do: ODBC.load_tables(connection, update_in(table.db_name, &~s/"#{&1}"/))

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: ODBC

  @impl Driver
  defdelegate driver_info(connection), to: ODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: ODBC
end
