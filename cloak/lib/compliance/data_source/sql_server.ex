defmodule Compliance.DataSource.SQLServer do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.{Connector, SQLServer.Queries}

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:odbc)
    conn = Cloak.DataSource.SQLServer.connect!(params)
    Enum.each(Queries.setup(), &execute!(conn, &1))
    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    Enum.each(Queries.create_table(table_name, columns), &execute!(conn, &1))
    conn
  end

  @impl Connector
  def insert_rows(table_name, data, conn) do
    Enum.each(Queries.insert_rows(table_name, data), &execute!(conn, &1))
    conn
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp execute!(conn, query, params \\ []) do
    case :odbc.param_query(conn, String.to_charlist(query), params) do
      {:updated, _} -> :ok
      {:error, error} -> raise to_string(error)
    end
  end
end
