defmodule Cloak.DataSource.SQLServer do
  @moduledoc "Implements the DataSource.Driver behaviour for MS SQL Server. For more information, see `DataSource`."

  alias Cloak.DataSource.RODBC
  use Cloak.DataSource.Driver.RodbcSql

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters) do
    with {:ok, connection} <- RODBC.connect(parameters, &conn_params/1) do
      :ok = RODBC.Port.execute(connection, "SET ANSI_DEFAULTS ON; SET ANSI_WARNINGS OFF; SET ARITHABORT OFF;")
      {:ok, connection}
    end
  end

  @impl Driver
  def load_comments(connection, table) do
    {schema_name, table_name} = table_name_parts(table)

    table_comment =
      connection
      |> select("""
        SELECT value
        FROM fn_listextendedproperty('MS_Description', 'SCHEMA', '#{schema_name}', 'TABLE', '#{table_name}', NULL, NULL)
      """)
      |> case do
        {:ok, results} ->
          results
          |> Enum.to_list()
          |> case do
            [[comment]] -> comment
            _ -> nil
          end

        _ ->
          nil
      end

    column_comments =
      connection
      |> select("""
        SELECT objname, value
        FROM fn_listextendedproperty('MS_Description', 'SCHEMA', '#{schema_name}', 'TABLE', '#{table_name}', 'COLUMN', NULL)
        WHERE value IS NOT NULL
      """)
      |> case do
        {:ok, results} ->
          results
          |> Enum.map(&List.to_tuple/1)
          |> Enum.into(%{})

        {:error, _} ->
          %{}
      end

    {table_comment, column_comments}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    %{
      DSN: "SQLServer",
      Server: normalized_parameters[:hostname],
      Uid: normalized_parameters[:username],
      Pwd: normalized_parameters[:password],
      Database: normalized_parameters[:database]
    }
  end

  defp table_name_parts(table) do
    case SqlBuilder.table_name_parts(table.db_name) do
      [full_table_name] -> {"dbo", full_table_name}
      [schema_name, table_name] -> {schema_name, table_name}
    end
  end
end
