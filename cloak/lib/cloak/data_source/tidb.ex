defmodule Cloak.DataSource.TiDB do
  @moduledoc """
  Implements the DataSource.Driver behaviour for TiDB.
  Delegates everything do MySQL. The reason it's kept separate is because we
  want do use a separate SqlBuilder for TiDB.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.MySQL
  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters), do: MySQL.connect(parameters)

  @impl Driver
  def load_tables(connection, table), do: MySQL.load_tables(connection, table)

  @impl Driver
  def select(connection, sql_query, result_processor), do: MySQL.select(connection, sql_query, result_processor)

  @impl Driver
  def driver_info(connection), do: MySQL.driver_info(connection)

  @impl Driver
  def supports_query?(query), do: MySQL.supports_query?(query)

  # -------------------------------------------------------------------
  # DataSource.Driver.SQL callbacks
  # -------------------------------------------------------------------

  @impl Driver.SQL
  def execute(connection, sql), do: MySQL.execute(connection, sql)

  @impl Driver.SQL
  def select(connection, sql), do: MySQL.select(connection, sql)
end
