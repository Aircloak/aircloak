defmodule Compliance.DataSource.SQLServerTds do
  @moduledoc false


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  use Compliance.DataSource.Connector
  alias Compliance.DataSource.{Connector, SQLServer.Common}

  @impl Connector
  def setup(%{parameters: params}) do
    Application.ensure_all_started(:tds)
    Connector.await_port(params.hostname, 1433)
    setup_database(params)
    conn = Cloak.DataSource.SQLServerTds.connect!(params)
    Enum.each(Common.setup_queries(), &execute!(conn, &1))
    conn
  end

  @impl Connector
  def create_table(table_name, columns, conn) do
    Enum.each(Common.create_table_queries(table_name, columns), &execute!(conn, &1))
    conn
  end

  @impl Connector
  def insert_rows(table_name, data, conn) do
    table_name
    |> chunks_to_insert(data)
    |> Enum.each(&execute!(conn, &1))

    conn
  end

  @impl Connector
  def terminate(_conn) do
    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp chunks_to_insert(table_name, data) do
    column_names = Common.column_names(data)

    data
    |> Common.rows(column_names)
    |> Stream.chunk_every(100)
    |> Stream.map(&chunk_to_insert(table_name, column_names, &1))
  end

  defp chunk_to_insert(table_name, column_names, rows) do
    escaped_column_names = Common.escaped_column_names(column_names)

    value_literals =
      rows
      |> Stream.map(&cast_types/1)
      # credo:disable-for-next-line Credo.Check.Readability.SpaceAfterCommas
      |> Stream.map(&"(#{Enum.join(&1, ",")})")
      |> Enum.join(", ")

    "
      INSERT INTO #{table_name}(#{Enum.join(escaped_column_names, ", ")})
      SELECT #{Enum.join(escaped_column_names, ", ")} FROM (
        VALUES #{value_literals}
      ) subquery (#{Enum.join(escaped_column_names, ", ")})
    "
  end

  defp cast_types(params), do: Enum.map(params, &cast_type/1)

  defp cast_type(binary) when is_binary(binary), do: "N'#{String.replace(binary, "'", "''")}'"
  defp cast_type(integer) when is_integer(integer), do: to_string(integer)
  defp cast_type(float) when is_float(float), do: to_string(float)
  defp cast_type(true), do: 1
  defp cast_type(false), do: 0
  defp cast_type(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_type()

  defp execute!(conn, query), do: Tds.query!(conn, query, [])

  defp setup_database(params) do
    {:ok, conn} =
      Tds.start_link(
        hostname: params.hostname,
        username: params.username,
        password: params.password,
        database: "master",
        sync_connect: true,
        pool: DBConnection.Connection
      )

    case execute!(conn, "select count(*) from sys.databases where name='#{params.database}'").rows do
      [[n]] when n > 0 -> execute!(conn, "drop database #{params.database}")
      [[0]] -> :ok
    end

    execute!(conn, "create database #{params.database}")
  end
end
