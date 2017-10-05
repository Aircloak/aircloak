defmodule Compliance.DataSource.SQLServer.Common do
  @moduledoc false

  @doc false
  def setup_queries(), do:
    ["SET IMPLICIT_TRANSACTIONS off"]

  @doc false
  def create_table_queries(table_name, columns), do:
    [
      "DROP TABLE IF EXISTS #{table_name}",
      "CREATE TABLE #{table_name} (#{columns_sql(columns)})"
    ]

  @doc false
  def insert_rows_queries(table_name, data) do
    column_names = column_names(data)
    rows = rows(data, column_names)
    escaped_column_names = escaped_column_names(column_names)

    Enum.map(rows, fn(row) ->
      "INSERT INTO #{table_name} (#{Enum.join(escaped_column_names, ", ")}) values (#{Enum.join(row, ", ")})"
    end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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

  defp cast_types(binary) when is_binary(binary), do: "'#{String.replace(binary, "'", "''")}'"
  defp cast_types(integer) when is_integer(integer), do: to_string(integer)
  defp cast_types(float) when is_float(float), do: to_string(float)
  defp cast_types(true), do: 1
  defp cast_types(false), do: 0
  defp cast_types(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_types()

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
  defp sql_type(:datetime), do: "datetime2"
end
