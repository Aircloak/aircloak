defmodule Compliance.DataSource.ClouderaImpala do
  @moduledoc false

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.Connector

  @impl Connector
  def setup(data_source) do
    Mix.Tasks.Cloak.PingDb.ping!(data_source, attempts: 60)
    :ok
  end

  @impl Connector
  def connect(%{parameters: params}) do
    create_connection!(params)
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    execute!(conn, "DROP TABLE IF EXISTS #{table_name}")
    execute!(conn, "CREATE TABLE #{table_name} (#{columns_sql(columns)}) COMMENT 'This is table #{table_name}.'")
    conn
  end

  @impl Connector
  def after_tables_created(state), do: state

  @impl Connector
  def insert_rows(table_name, data, conn) do
    column_names = column_names(data)

    data
    |> rows(column_names)
    |> Stream.chunk_every(100)
    |> Enum.each(&insert_chunk(conn, table_name, column_names, &1))
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end

  @impl Connector
  def adjust_data_source(data_source), do: super(data_source)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp create_connection!(params) do
    conn_string =
      [
        DSN: "Cloudera Impala",
        HOST: params.hostname,
        PORT: params.port,
        Schema: params.database || "default",
        UID: params.username,
        PWD: params.password,
        charset: "UTF8"
      ]
      |> Enum.reject(&match?({_key, nil}, &1))
      |> Enum.map(fn {key, value} -> [to_string(key), ?=, to_string(value)] end)
      |> Enum.join(";")

    :odbc.connect(to_charlist(conn_string), binary_strings: :on, tuple_row: :off)
    |> case do
      {:ok, connection} ->
        connection

      {:error, error} ->
        raise to_string(error)
    end
  end

  defp insert_chunk(conn, table_name, column_names, rows) do
    values =
      rows
      |> Stream.concat()
      |> Stream.map(fn _ -> "?" end)
      |> Stream.chunk_every(length(hd(rows)))
      # credo:disable-for-next-line Credo.Check.Readability.SpaceAfterCommas
      |> Stream.map(&"(#{Enum.join(&1, ",")})")
      |> Enum.join(", ")

    query = "INSERT INTO #{table_name} (#{Enum.join(escaped_column_names(column_names), ", ")}) VALUES #{values}"
    execute!(conn, query, List.flatten(rows))
  end

  defp cast_type(binary) when is_binary(binary) do
    # Impala complains with encodings different from UTF8
    {{:sql_varchar, byte_size(binary) + 1}, [binary]}
  end

  defp cast_type(integer) when is_integer(integer), do: {:sql_integer, [integer]}
  defp cast_type(float) when is_float(float), do: {:sql_double, [float]}
  defp cast_type(boolean) when is_boolean(boolean), do: {:sql_bit, [boolean]}
  defp cast_type(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_type()
  defp cast_type(nil), do: {{:sql_wvarchar, 10}, [:null]}

  defp execute!(conn, query, params \\ []) do
    case :odbc.param_query(conn, String.to_charlist(query), Enum.map(params, &cast_type/1)) do
      {:updated, _} -> :ok
      {:error, error} -> raise to_string(error)
    end
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

  defp column_sql({name, type}), do: "#{escape_name(name)} #{sql_type(type)} COMMENT 'This is column #{name}.'"

  defp escape_name(name), do: "`#{name}`"

  defp sql_type(:integer), do: "BIGINT"
  defp sql_type(:real), do: "DOUBLE"
  defp sql_type(:boolean), do: "BOOLEAN"
  defp sql_type(:text), do: "STRING"
  defp sql_type(:datetime), do: "TIMESTAMP"
  defp sql_type(:date), do: "TIMESTAMP"
end
