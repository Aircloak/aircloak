defmodule Compliance.DataSource.SQLServer do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.{Connector, SQLServer.Common}

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:odbc)
    Connector.await_port(params.hostname, 1433)
    setup_database(params)
    conn = Cloak.DataSource.SQLServer.connect!(params)
    Enum.each(Common.setup_queries(), &execute!(conn, &1))
    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    Enum.each(Common.create_table_queries(table_name, columns), &execute!(conn, &1))
    conn
  end

  @impl Connector
  def insert_rows(table_name, data, conn) do
    {sql, rows} = Common.insert_rows_query(table_name, data)
    placeholders = List.duplicate("?", rows |> Enum.at(0) |> length()) |> Enum.join(", ")
    sql = String.replace(sql, "$VALUES", placeholders)
    Enum.each(rows, &execute!(conn, sql, &1))
    conn
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

   defp cast_type(binary) when is_binary(binary) do
    binary = :unicode.characters_to_binary(binary, :utf8, {:utf16, :little})
     {{:sql_wvarchar, byte_size(binary) + 1}, [binary]}
   end
   defp cast_type(integer) when is_integer(integer), do: {:sql_integer, [integer]}
   defp cast_type(float) when is_float(float), do: {:sql_real, [float]}
   defp cast_type(boolean) when is_boolean(boolean), do: {:sql_bit, [boolean]}
   defp cast_type(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_type()

  defp execute!(conn, query, params \\ []) do
    case :odbc.param_query(conn, String.to_charlist(query), Enum.map(params, &cast_type/1)) do
      {:updated, _} -> :ok
      {:error, error} -> raise to_string(error)
    end
  end

  defp setup_database(params) do
    conn =
      Cloak.DataSource.SQLServer.connect!(
        hostname: params.hostname,
        database: "master",
        username: "sa",
        password: "7fNBjlaeoRwz*zH9"
      )

    :odbc.sql_query(conn, ~c/
      IF EXISTS(select * from sys.databases where name='#{params.database}')
        DROP DATABASE #{params.database}

      CREATE DATABASE #{params.database}
    /)
  end
end
