defmodule Cloak.DataSource.SAPIQ do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP IQ.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.ODBC
  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_), do: Cloak.DataSource.SqlBuilder.SAPIQ

  @impl Driver
  def connect!(parameters), do: ODBC.connect!(parameters, &conn_params/1)

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

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    %{
      Host: "#{normalized_parameters[:hostname]}:#{normalized_parameters[:port]}",
      UserID: normalized_parameters[:username],
      Password: normalized_parameters[:password],
      DatabaseName: normalized_parameters[:database],
      DSN: "SAPIQ"
    }
  end
end
