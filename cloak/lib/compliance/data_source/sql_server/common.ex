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
  def column_names(data), do:
    data
    |> hd()
    |> Map.keys()
    |> Enum.sort()

  @doc false
  def rows(data, column_names), do:
    Enum.map(data, fn(entry) ->
      Enum.map(column_names, &Map.get(entry, &1))
    end)

  @doc false
  def escaped_column_names(column_names), do:
    column_names
    |> Enum.map(&Atom.to_string/1)
    |> Enum.map(&escape_name/1)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

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
