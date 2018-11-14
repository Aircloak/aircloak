defmodule Cloak.DataSource.Oracle do
  @moduledoc """
  Implements the DataSource.Driver behaviour for the Oracle Database, targeting version g11 R2.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  alias Cloak.DataSource.{RODBC, SqlBuilder}

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(), do: SqlBuilder.Oracle

  @impl Driver
  def connect(parameters) do
    :jamdb_oracle.start_link(
      host: to_charlist(parameters.hostname),
      port: parameters[:port],
      user: to_charlist(parameters.username),
      password: to_charlist(parameters.password),
      sid: to_charlist(parameters.sid)
    )
    |> case do
      {:ok, conn} -> {:ok, conn}
      error -> {:error, inspect(error)}
    end
  end

  @impl Driver
  def disconnect(connection), do: :jamdb_oracle.stop(connection)

  @impl Driver
  def load_tables(connection, table) do
    table = update_in(table.db_name, &SqlBuilder.quote_table_name(&1, ?"))
    RODBC.load_tables(connection, table, &"SELECT * FROM #{&1} LIMIT 1")
  end

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: RODBC

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  @impl Driver
  def supports_query?(query),
    do: query |> get_in([Cloak.Sql.Query.Lenses.joins()]) |> Enum.any?(&(&1.type == :cross_join)) |> :erlang.not()
end
