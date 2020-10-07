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
  defdelegate connect(parameters), to: MySQL

  @impl Driver
  defdelegate load_tables(connection, table), to: MySQL

  @impl Driver
  defdelegate load_comments(connection, table), to: MySQL

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: MySQL

  @impl Driver
  defdelegate driver_info(connection), to: MySQL

  @impl Driver
  defdelegate supports_query?(query), to: MySQL

  # -------------------------------------------------------------------
  # DataSource.Driver.SQL callbacks
  # -------------------------------------------------------------------

  @impl Driver.SQL
  defdelegate execute(connection, sql), to: MySQL

  @impl Driver.SQL
  defdelegate select(connection, sql), to: MySQL
end
