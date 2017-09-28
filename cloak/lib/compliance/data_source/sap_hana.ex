defmodule Compliance.DataSource.SAPHana do
  @moduledoc false

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector
  alias Cloak.SapHanaHelpers


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:odbc)
    {:ok, conn} = SapHanaHelpers.connect(params)
    SapHanaHelpers.ensure_schema!(conn, schema())
    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    # We'll prefix the compliance table names, to avoid clashes with other tables in the same schema, for example the
    # tables used in unit tests.
    SapHanaHelpers.recreate_table!(conn, schema(), db_table_name(table_name), columns_sql(columns))
    conn
  end

  @impl Connector
  def insert_rows(table_name, data, conn) do
    column_names = column_names(data)
    rows = rows(data, column_names)
    SapHanaHelpers.insert_rows!(conn, schema(), db_table_name(table_name), column_names, rows)
    conn
  end

  @impl Connector
  def terminate(conn), do:
    :odbc.disconnect(conn)

  @impl Connector
  def db_table_name(table_name), do:
    ~s/compliance.#{table_name}/


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp schema() do
    case Cloak.DataSource.SAPHana.default_schema() do
      nil -> raise "Default schema for SAP HANA not configured."
      schema -> schema
    end
  end

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
  defp sql_type(:text), do: "nclob"
  defp sql_type(:datetime), do: "timestamp"

  defp column_names(data), do:
    data
    |> hd()
    |> Map.keys()
    |> Enum.sort()

  defp rows(data, column_names), do:
    Enum.map(data, fn(entry) ->
      Enum.map(column_names, &to_literal(Map.get(entry, &1)))
    end)

  defp to_literal(value) do
    cond do
      is_nil(value) -> "NULL"
      is_binary(value) -> "'#{String.replace(value, "'", "''")}'"
      is_number(value) -> to_string(value)
      is_boolean(value) -> to_string(value)
      match?(%NaiveDateTime{}, value) -> "timestamp'#{to_string(value)}'"
      true -> raise "Unsupported value #{inspect value}"
    end
  end
end
