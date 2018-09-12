defmodule Cloak.DataSource.SAPIQ do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP IQ.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.ODBC
  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Returns the prefix of all tables.

  This allows us to simulate schemas (which don't exist in SAP IQ), and thus run concurrent compliance tests.
  """
  @spec table_prefix() :: String.t() | nil
  def table_prefix() do
    case System.get_env("GLOBAL_DB_NAMESPACE") do
      nil -> nil
      "" -> nil
      prefix -> prefix
    end
  end

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
  defdelegate load_tables(connection, table), to: ODBC

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
