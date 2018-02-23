defmodule Compliance.DataSource.PostgreSQL do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:postgrex)
    Connector.await_port(params.hostname, Map.get(params, :port, 5432))
    setup_database(params)
    :ok
  end

  @impl Connector
  def connect(%{parameters: params}) do
    {:ok, conn} = Postgrex.start_link(
      database: params.database,
      hostname: params.hostname,
      port: Map.get(params, :port, 5432),
      username: params.username,
      password: params[:password]
    )
    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    execute!(conn, "DROP TABLE IF EXISTS #{table_name}")
    execute!(conn, "CREATE TABLE #{table_name} (#{columns_sql(columns)})")
    conn
  end

  @impl Connector
  def insert_rows(table_name, data, conn) do
    column_names = column_names(data)

    data
    |> rows(column_names)
    |> Stream.chunk_every(100)
    |> Enum.each(&insert_chunk(conn, table_name, column_names, &1))

    conn
  end

  defp insert_chunk(conn, table_name, column_names, rows) do
    placeholders =
      rows
      |> Stream.concat()
      |> Stream.with_index(1)
      |> Stream.map(fn({_value, index}) -> "$#{index}" end)
      |> Stream.chunk_every(length(hd(rows)))
      # credo:disable-for-next-line Credo.Check.Readability.SpaceAfterCommas
      |> Stream.map(&"(#{Enum.join(&1, ",")})")
      |> Enum.join(", ")

    query = "INSERT INTO #{table_name} (#{Enum.join(escaped_column_names(column_names), ", ")}) values #{placeholders}"
    Postgrex.query!(conn, query, List.flatten(rows))
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp execute!(conn, query, params \\ []), do:
    Postgrex.query!(conn, query, params)

  defp column_names(data), do:
    data
    |> hd()
    |> Map.keys()
    |> Enum.sort()

  defp rows(data, column_names), do:
    Enum.map(data, fn(entry) ->
      Enum.map(column_names, & Map.get(entry, &1))
    end)

  defp escaped_column_names(column_names), do:
    column_names
    |> Enum.map(& Atom.to_string/1)
    |> Enum.map(& "\"#{&1}\"")

  defp columns_sql(columns), do:
    columns
    |> Enum.map(& column_sql/1)
    |> Enum.join(", ")

  defp column_sql({name, type}), do:
    "#{escape_name(name)} #{sql_type(type)}"

  defp escape_name(name), do:
    "\"#{name}\""

  defp sql_type(:integer), do: "integer"
  defp sql_type(:real), do: "float"
  defp sql_type(:boolean), do: "boolean"
  defp sql_type(:text), do: "text"
  defp sql_type(:datetime), do: "timestamp"

  defp setup_database(params) do
    {:ok, conn} = Postgrex.start_link(
      database: "postgres",
      hostname: params.hostname,
      port: Map.get(params, :port, 5432),
      username: "postgres",
      sync_connect: true
    )

    case Postgrex.query!(
      conn,
      "SELECT count(*) FROM pg_catalog.pg_user WHERE usename = '#{params.username}'",
      []
    ).rows do
      [[1]] -> :ok
      [[0]] -> Postgrex.query!(conn, "CREATE USER #{params.username}", [])
    end

    Postgrex.query!(conn, "DROP DATABASE IF EXISTS #{params.database}", [])
    Postgrex.query!(conn, "CREATE DATABASE #{params.database} ENCODING 'UTF8'", [])
    Postgrex.query!(conn, "GRANT ALL PRIVILEGES ON DATABASE #{params.database} TO #{params.username}", [])
  end
end
