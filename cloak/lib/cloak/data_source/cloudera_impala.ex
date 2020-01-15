defmodule Cloak.DataSource.ClouderaImpala do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Cloudera Data Platform (CDP) Impala.
  This targets version 5.13.0 of CDP.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.RODBC
  use Cloak.DataSource.Driver.RodbcSql
  @default_port 21050

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters), do: RODBC.connect(parameters, &conn_params/1)

  @impl Driver
  def load_tables(connection, table),
    do:
      connection
      |> RODBC.load_tables(table)
      |> Enum.map(&update_column_names/1)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_column_names(table),
    do: %{table | columns: Enum.map(table.columns, &update_column_name(table.db_name, &1))}

  defp update_column_name(table_name, column),
    do: %{column | name: String.replace_prefix(column.name, "#{table_name}.", "")}

  defp conn_params(normalized_parameters) do
    %{
      DSN: "Cloudera Impala",
      HOST: normalized_parameters[:hostname],
      PORT: Map.get(normalized_parameters, :port, @default_port),
      Schema: normalized_parameters.database || "default",
      UID: normalized_parameters[:username],
      PWD: normalized_parameters[:password]
    }
  end
end
