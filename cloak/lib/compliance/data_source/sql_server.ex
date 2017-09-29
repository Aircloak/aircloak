defmodule Compliance.DataSource.SQLServer do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:odbc)
    conn = Cloak.DataSource.SQLServer.connect!(params)
    {:updated, _} = :odbc.param_query(conn, 'SET IMPLICIT_TRANSACTIONS off', [])
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

    Enum.each(rows, & execute!(conn, query, &1))

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

  defp cast_types(binary) when is_binary(binary) do
    binary = :unicode.characters_to_binary(binary, :utf8, {:utf16, :little})
    {{:sql_wvarchar, length_null_terminated(binary)}, [binary]}
  end
  defp cast_types(integer) when is_integer(integer), do: {:sql_integer, [integer]}
  defp cast_types(float) when is_float(float), do: {:sql_real, [float]}
  defp cast_types(boolean) when is_boolean(boolean), do: {:sql_bit, [boolean]}
  defp cast_types(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_types()

  defp length_null_terminated(binary), do: byte_size(binary) + 1

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
  defp sql_type(:text), do: "nvarchar(4000)"
  defp sql_type(:datetime), do: "datetime2"
end
