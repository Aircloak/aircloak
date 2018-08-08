defmodule Cloak.DataSource.Drill do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Apache Drill.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.ODBC
  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # API
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
  def sql_dialect_module(_), do: Cloak.DataSource.SqlBuilder.Drill

  @impl Driver
  def connect!(parameters), do: ODBC.connect!(parameters, &conn_params/1)

  @impl Driver
  defdelegate disconnect(connection), to: ODBC

  @impl Driver
  def load_tables(connection, table), do: ODBC.load_tables(connection, update_in(table.db_name, &"`#{&1}`"))

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: ODBC

  @impl Driver
  defdelegate driver_info(connection), to: ODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: ODBC
end
