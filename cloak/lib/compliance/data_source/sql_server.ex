defmodule Compliance.DataSource.SQLServer do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Compliance.DataSource.Connector

  @doc false
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:odbc)
    conn = Cloak.DataSource.SQLServer.connect!(params)
    {:updated, _} = :odbc.param_query(conn, 'SET IMPLICIT_TRANSACTIONS off', [])
    conn
  end

  @doc false
  def create_table(table_name, columns, conn) do
    execute!(conn, "DROP TABLE IF EXISTS #{table_name}")
    execute!(conn, "CREATE TABLE #{table_name} (#{columns_sql(columns)})")
    conn
  end

  @doc false
  def insert_data(table_name, data, conn) do
    column_names = column_names(data)
    rows = rows(data, column_names)
    escaped_column_names = escaped_column_names(column_names)
    value_placeholders = List.duplicate("?", length(column_names)) |> Enum.join(", ")

    query = "INSERT INTO #{table_name} (#{Enum.join(escaped_column_names, ", ")}) values (#{value_placeholders})"

    Task.async_stream(rows, & execute!(conn, query, &1))

    conn
  end

  @doc false
  def terminate(_conn) do
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp execute!(conn, query, params \\ []), do:
    {:updated, _} = :odbc.param_query(conn, String.to_charlist(query), params)

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
    ~s("#{name}")

  defp sql_type(:integer), do: "integer"
  defp sql_type(:real), do: "real"
  defp sql_type(:boolean), do: "bit"
  defp sql_type(:text), do: "text"
  defp sql_type(:datetime), do: "datetime"
end
