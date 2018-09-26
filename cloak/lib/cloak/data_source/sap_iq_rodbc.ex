defmodule Cloak.DataSource.SAPIQRODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP IQ using the Rust ODBC port driver..
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  alias Cloak.DataSource
  alias Cloak.DataSource.RODBC

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
  def sql_dialect_module(), do: Cloak.DataSource.SqlBuilder.SAPIQ

  @impl Driver
  def connect!(parameters) do
    unless File.exists?(driver_path()), do: DataSource.raise_error("ODBC driver for SAP IQ is not mounted.")
    RODBC.connect!(parameters, &conn_params/1, wstr_as_bin: true)
  end

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table),
    do: RODBC.load_tables(connection, update_in(table.db_name, &SqlBuilder.quote_table_name/1))

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: RODBC

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: RODBC

  @impl Driver
  defdelegate cast_to_text?(), to: RODBC

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp driver_path(), do: Path.join(~w(#{Application.app_dir(:cloak)} priv odbc drivers sapiq lib64 libdbodbc.so))

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
