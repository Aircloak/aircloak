defmodule Compliance.DataSource.Oracle do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(%{parameters: params}) do
    {:ok, _} = Application.ensure_all_started(:jamdb_oracle)
    Connector.await_port(params.hostname, Map.get(params, :port, 1521))
    :ok
  end

  @impl Connector
  def connect(%{parameters: params}) do
    {:ok, conn} =
      :jamdb_oracle.start_link(
        host: to_charlist(params.hostname),
        port: params[:port],
        user: to_charlist(params.username),
        password: to_charlist(params.password),
        sid: to_charlist(params.sid)
      )

    execute(conn, "COMON")

    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    drop_table!(conn, table_name)
    execute!(conn, "CREATE TABLE #{table_name} (#{columns_sql(columns)})")
    conn
  end

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(table_name, data, conn) do
    # table_name
    # |> chunks_to_insert(data)
    # |> Enum.each(fn {sql, params} -> execute!(conn, sql, params) end)

    # conn
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp drop_table!(conn, table_name) do
    case execute(conn, "DROP TABLE #{table_name}") do
      {:ok, [affected_rows: 0]} -> :ok
      {:ok, [{:proc_result, _table_does_not_exist = 942, _}]} -> :ok
      other -> raise inspect(other)
    end
  end

  defp execute!(conn, query), do: {:ok, [affected_rows: _]} = execute(conn, query)

  defp execute(conn, query), do: :jamdb_oracle.sql_query(conn, to_charlist(query))

  def chunks_to_insert(table_name, data) do
    column_names = column_names(data)

    data
    |> rows(column_names)
    |> Stream.chunk_every(100)
    |> Enum.map(&chunk_to_insert(table_name, column_names, &1))
  end

  defp chunk_to_insert(table_name, column_names, rows) do
    columns = column_names |> escaped_column_names() |> Enum.join(", ")
    row_placeholders = column_names |> Stream.map(fn _column -> "?" end) |> Enum.join(",")

    all_placeholders = rows |> Stream.map(fn _row -> "(#{row_placeholders})" end) |> Enum.join(", ")

    query = "
      INSERT INTO #{table_name}(#{columns})
      SELECT #{columns} FROM (VALUES #{all_placeholders}) subquery (#{columns})
    "

    {query, List.flatten(rows)}
  end

  defp cast_type(binary) when is_binary(binary) do
    binary = :unicode.characters_to_binary(binary, :utf8, {:utf16, :little})
    {{:sql_wvarchar, byte_size(binary) + 1}, [binary]}
  end

  defp cast_type(integer) when is_integer(integer), do: {:sql_integer, [integer]}
  defp cast_type(float) when is_float(float), do: {:sql_double, [float]}
  defp cast_type(boolean) when is_boolean(boolean), do: {:sql_bit, [boolean]}
  defp cast_type(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_type()
  defp cast_type(nil), do: {{:sql_wvarchar, 10}, [:null]}

  defp setup_database(params) do
    conn = create_connection!(%{params | database: "master"})
    :odbc.sql_query(conn, ~c/
      IF EXISTS(select * from sys.databases where name='#{params.database}')
        DROP DATABASE #{params.database}

      CREATE DATABASE #{params.database}
    /)
  end

  defp column_names(data),
    do:
      data
      |> hd()
      |> Map.keys()
      |> Enum.sort()

  defp rows(data, column_names),
    do:
      Enum.map(data, fn entry ->
        Enum.map(column_names, &Map.get(entry, &1))
      end)

  defp escaped_column_names(column_names),
    do:
      column_names
      |> Enum.map(&Atom.to_string/1)
      |> Enum.map(&escape_name/1)

  defp columns_sql(columns),
    do:
      columns
      |> Enum.map(&column_sql/1)
      |> Enum.join(", ")

  defp column_sql({name, type}), do: "#{escape_name(name)} #{sql_type(type)}"

  defp escape_name(name), do: ~s("#{name}")

  defp sql_type(:integer), do: "NUMBER"
  defp sql_type(:real), do: "BINARY_FLOAT"
  defp sql_type(:boolean), do: "NUMBER(1)"
  defp sql_type(:text), do: "VARCHAR2(4000)"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:date), do: "DATE"
end
