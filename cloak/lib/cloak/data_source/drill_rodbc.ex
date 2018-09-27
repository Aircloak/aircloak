defmodule Cloak.DataSource.DrillRODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Apache Drill using the Rust ODBC port driver.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  alias Cloak.DataSource.{RODBC, SqlBuilder}

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @doc "Converts the connection parameters from the config format into the ODBC format."
  @spec conn_params(Map.t()) :: Map.t()
  def conn_params(normalized_parameters) do
    params = %{
      HOST: normalized_parameters[:hostname],
      Schema: normalized_parameters[:database],
      DSN: "MapRDrill"
    }

    port = normalized_parameters[:port]
    params = if port != nil and port != 0, do: Map.put_new(params, :PORT, port), else: params

    if normalized_parameters[:username] != nil do
      Map.merge(params, %{
        UID: normalized_parameters[:username],
        PWD: normalized_parameters[:password],
        AuthenticationType: "Plain"
      })
    else
      params
    end
  end

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(), do: SqlBuilder.Drill

  @impl Driver
  def connect!(parameters), do: RODBC.connect!(parameters, &conn_params/1)

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table),
    do: RODBC.load_tables(connection, update_in(table.db_name, &SqlBuilder.quote_table_name(&1, ?`)))

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: RODBC

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: RODBC

  @impl Driver
  defdelegate cast_to_text?(), to: RODBC

  @impl Driver
  def supports_query?(query),
    do: query |> get_in([Cloak.Sql.Query.Lenses.joins()]) |> Enum.any?(&(&1.type == :cross_join)) |> :erlang.not()
end
