defmodule Compliance.DataSource.PostgreSQL do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Compliance.DataSource.Connector

  @doc false
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:postgrex)
    {:ok, conn} = Postgrex.start_link(
      database: params.database,
      hostname: params.hostname,
      username: params.username,
      password: params[:password]
    )
    conn
  end

  @doc false
  def create_table(table_name, columns, conn) do
    execute!(conn, "DROP TABLE IF EXISTS #{table_name}")
    execute!(conn, "CREATE TABLE #{table_name} (#{columns_sql(columns)})")
    conn
  end

  @doc false
  def insert_rows(table_name, data, conn) do
    column_names = column_names(data)
    rows = rows(data, column_names)
    escaped_column_names = escaped_column_names(column_names)
    indexed_sql = (1..length(column_names))
    |> Enum.to_list()
    |> Enum.map(& "$#{&1}")
    |> Enum.join(", ")

    query = "INSERT INTO #{table_name} (#{Enum.join(escaped_column_names, ", ")}) values (#{indexed_sql})"

    Task.async_stream(rows, & execute!(conn, query, &1))

    conn
  end

  @doc false
  def insert_documents(_collection_name, _documents, conn), do: conn

  @doc false
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
end
