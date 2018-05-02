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
  def connect!(parameters) do
    normalized_parameters =
      for {key, value} <- parameters,
          into: %{},
          do: {key |> Atom.to_string() |> String.downcase() |> String.to_atom(), value}

    odbc_parameters =
      %{
        Host: "#{normalized_parameters[:hostname]}:#{normalized_parameters[:port]}",
        UserID: normalized_parameters[:username],
        Password: normalized_parameters[:password],
        DatabaseName: normalized_parameters[:database],
        DSN: "SAPIQ"
      }
      |> add_optional_parameters(parameters)

    ODBC.connect!(odbc_parameters)
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

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # Allows for adding additional ODBC connection parameters in the case where
  # a SQL Server installation requires additional parameters.
  defp add_optional_parameters(default_params, %{odbc_parameters: additonal_parameters}),
    do: Map.merge(default_params, additonal_parameters)

  defp add_optional_parameters(default_params, _), do: default_params
end
