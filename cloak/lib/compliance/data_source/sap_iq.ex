defmodule Compliance.DataSource.SAPIQ do
  @moduledoc false

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector
  alias Cloak.SapIqHelper

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Connector
  def setup(_datasource) do
    Application.ensure_all_started(:odbc)
    :ok
  end

  @impl Connector
  def connect(%{parameters: params}) do
    {:ok, conn} = SapIqHelper.connect(params)
    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn),
    do: SapIqHelper.recreate_table!(conn, db_table_name(table_name), columns_sql(columns))

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(table_name, data, conn) do
    column_names = column_names(data)
    rows = rows(data, column_names)
    SapIqHelper.insert_rows!(conn, db_table_name(table_name), column_names, rows)
  end

  @impl Connector
  def terminate(conn), do: :odbc.disconnect(conn)

  @impl Connector
  def db_table_name(table_name), do: "#{table_prefix()}_#{table_name}"

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp table_prefix() do
    with nil <- Cloak.DataSource.SAPIQ.table_prefix() do
      raise """
        Please set the GLOBAL_DB_NAMESPACE OS variable. If you're developing locally, then use a unique repeatable
        value, such as your first name. This value will be used as a prefix of the table names in the database.
      """
    end
  end

  defp columns_sql(columns), do: columns |> Enum.map(&column_sql/1) |> Enum.join(", ")

  defp column_sql({name, type}), do: "#{escape_name(name)} #{sql_type(type)}"

  defp escape_name(name), do: "\"#{name}\""

  defp sql_type(:integer), do: "integer"
  defp sql_type(:real), do: "float"
  defp sql_type(:boolean), do: "bit"
  defp sql_type(:text), do: "nvarchar(32767)"
  defp sql_type(:datetime), do: "datetime"

  defp column_names(data), do: data |> hd() |> Map.keys() |> Enum.sort()

  defp rows(data, column_names), do: Enum.map(data, fn entry -> Enum.map(column_names, &Map.get(entry, &1)) end)
end
