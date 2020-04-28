defmodule Cloak.DataSource.Snowflake do
  @moduledoc """
  Implements the DataSource.Driver behaviour for the Oracle Database, targeting version 12c.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.RodbcSql
  use Cloak.DataSource.Driver.SQL.AnalystTables
  require Logger
  alias Cloak.DataSource.{RODBC, SqlBuilder}
  alias Cloak.Sql.Expression

  @mathematical_operators ~w(+ - * / ^)

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters) do
    with {:ok, connection} <- RODBC.connect(parameters, &conn_params/1) do
      execute!(connection, "SET quote_character = '\"'")
      {:ok, connection}
    end
  end

  @impl Driver
  def health_check(connection) do
    case select(connection, "SELECT 1") do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @impl Driver
  def load_tables(connection, table) do
    connection
    |> RODBC.load_tables(table)
  end

  @impl Driver
  def select(connection, query, result_processor) do
    statement = SqlBuilder.build(query) |> IO.inspect()
    RODBC.select(connection, statement, query.db_columns, %{}, result_processor)
  end

  @impl Driver
  def supports_function?(expression = %Expression{kind: :function, name: name}, data_source) do
    if name in @mathematical_operators do
      Map.get(data_source[:parameters], :aircloak_udfs, false)
    else
      SqlBuilder.Support.supports_function?(expression, data_source)
    end
  end

  @impl Driver
  def supports_analyst_tables?(), do: true

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    %{
      Host: normalized_parameters[:hostname],
      Uid: normalized_parameters[:username],
      Pwd: normalized_parameters[:password],
      database: normalized_parameters[:database],
      role: normalized_parameters[:role],
      warehouse: normalized_parameters[:warehouse],
      DSN: "Snowflake"
    }
    |> IO.inspect()
  end
end
