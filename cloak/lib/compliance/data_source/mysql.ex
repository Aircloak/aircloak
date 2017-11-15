defmodule Compliance.DataSource.MySQL do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:mariaex)
    Connector.await_port(params.hostname, 3306)
    setup_database(params)
    {:ok, conn} = Mariaex.start_link(
      database: params.database,
      hostname: params.hostname,
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
    rows = rows(data, column_names)
    escaped_column_names = escaped_column_names(column_names)
    value_placeholders = List.duplicate("?", length(column_names)) |> Enum.join(", ")

    query = "INSERT INTO #{table_name} (#{Enum.join(escaped_column_names, ", ")}) values (#{value_placeholders})"

    rows
    |> Task.async_stream(& execute!(conn, query, &1), timeout: :timer.minutes(1))
    |> Stream.run()

    conn
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp execute!(conn, query, params \\ []), do:
    Mariaex.query!(conn, query, params)

  defp column_names(data), do:
    data
    |> hd()
    |> Map.keys()
    |> Enum.sort()

  defp rows(data, column_names), do:
    Enum.map(data, fn(entry) ->
      column_names
      |> Enum.map(& Map.get(entry, &1))
      |> Enum.map(& cast_types/1)
    end)

  defp cast_types(%{calendar: Calendar.ISO} = datetime), do: NaiveDateTime.to_erl(datetime)
  defp cast_types(value), do: value

  defp escaped_column_names(column_names), do:
    column_names
    |> Enum.map(& Atom.to_string/1)
    |> Enum.map(& escape_name/1)

  defp columns_sql(columns), do:
    columns
    |> Enum.map(& column_sql/1)
    |> Enum.join(", ")

  defp column_sql({name, type}), do:
    "#{escape_name(name)} #{sql_type(type)}"

  defp escape_name(name), do:
    "`#{name}`"

  defp sql_type(:integer), do: "integer"
  defp sql_type(:real), do: "real"
  defp sql_type(:boolean), do: "boolean"
  defp sql_type(:text), do: "text"
  defp sql_type(:datetime), do: "timestamp"

  defp setup_database(params) do
    {:ok, conn} =
      Mariaex.start_link(
        database: "mysql",
        hostname: params.hostname,
        username: "root",
        sync_connect: true,
      )

    case Mariaex.query!(
      conn,
      "SELECT COUNT(*) FROM INFORMATION_SCHEMA.SCHEMATA WHERE SCHEMA_NAME = '#{params.database}'"
    ).rows do
      [[1]] -> :ok
      [[0]] -> Mariaex.query!(conn, "CREATE DATABASE #{params.database}")
    end
  end
end
