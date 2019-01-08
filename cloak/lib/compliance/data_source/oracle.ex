defmodule Compliance.DataSource.Oracle do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(%{parameters: params}) do
    {:ok, _} = Application.ensure_all_started(:odbc)
    Connector.await_port(params.hostname, Map.get(params, :port, 1521))
    :ok
  end

  @impl Connector
  def connect(%{parameters: params}) do
    conn = create_connection!(params)
    execute(conn, "COMON")
    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    drop_table!(conn, table_name)
    execute!(conn, "CREATE TABLE \"#{table_name}\" (#{columns_sql(columns)})")
    conn
  end

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(table_name, data, conn) do
    table_name
    |> rows_to_insert(data)
    |> Enum.each(fn {sql, params} -> execute!(conn, sql, params) end)

    conn
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp create_connection!(params) do
    [
      DSN: "Oracle",
      DBQ: "#{params.hostname}:#{Map.get(params, :port, 1521)}/#{params.database}",
      Uid: params.username,
      Pwd: params.password
    ]
    |> Stream.map(fn {key, value} -> [to_string(key), ?=, value] end)
    |> Enum.join(";")
    |> to_charlist()
    |> :odbc.connect(scrollable_cursors: :off, binary_strings: :on, tuple_row: :off)
    |> case do
      {:ok, connection} -> connection
      {:error, error} -> raise error
    end
  end

  defp drop_table!(conn, table_name) do
    case execute(conn, "SELECT COUNT(*) FROM ALL_TAB_COLUMNS WHERE table_name = '#{table_name}'") do
      {:ok, [{:result_set, [_], [], [[{0}]]}]} -> :ok
      _ -> execute(conn, "DROP TABLE \"#{table_name}\"")
    end
  end

  defp execute!(conn, query, params \\ []), do: {:updated, _} = execute(conn, query, params)

  defp execute(conn, query, params \\ []),
    do: :odbc.param_query(conn, String.to_charlist(query), Enum.map(params, &cast_type/1))

  def rows_to_insert(table_name, data) do
    column_names = column_names(data)

    data
    |> rows(column_names)
    |> Stream.map(&row_insert_data(table_name, column_names, &1))
  end

  defp row_insert_data(table_name, column_names, row) do
    columns = column_names |> escaped_column_names() |> Enum.join(", ")
    row_placeholders = column_names |> Stream.map(fn _column -> "?" end) |> Enum.join(",")
    query = ~s/INSERT INTO "#{table_name}"(#{columns}) SELECT #{row_placeholders} FROM DUAL/
    {query, row}
  end

  defp cast_type(binary) when is_binary(binary) do
    binary = :unicode.characters_to_binary(binary, :utf8, {:utf16, :little})
    {{:sql_wvarchar, byte_size(binary) + 1}, [binary]}
  end

  defp cast_type(integer) when is_integer(integer), do: {:sql_integer, [integer]}
  defp cast_type(float) when is_float(float), do: {:sql_double, [float]}
  defp cast_type(boolean) when is_boolean(boolean), do: {:sql_bit, [boolean]}
  defp cast_type(%NaiveDateTime{} = datetime), do: {:sql_timestamp, [NaiveDateTime.to_erl(datetime)]}
  defp cast_type(%Date{} = date), do: {:sql_timestamp, [{Date.to_erl(date), {0, 0, 0}}]}
  defp cast_type(nil), do: {{:sql_wvarchar, 10}, [:null]}

  defp column_names(data), do: data |> hd() |> Map.keys() |> Enum.sort()

  defp rows(data, column_names) do
    Enum.map(data, fn entry ->
      Enum.map(column_names, &Map.get(entry, &1))
    end)
  end

  defp escaped_column_names(column_names) do
    column_names
    |> Enum.map(&Atom.to_string/1)
    |> Enum.map(&escape_name/1)
  end

  defp columns_sql(columns) do
    columns
    |> Enum.map(&column_sql/1)
    |> Enum.join(", ")
  end

  defp column_sql({name, type}), do: "#{escape_name(name)} #{sql_type(type)}"

  defp escape_name(name), do: ~s("#{name}")

  defp sql_type(:integer), do: "NUMBER"
  defp sql_type(:real), do: "BINARY_DOUBLE"
  defp sql_type(:boolean), do: "NUMBER(1)"
  defp sql_type(:text), do: "VARCHAR2(4000)"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:date), do: "DATE"
end
