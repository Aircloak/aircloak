defmodule Air.Service.ShadowDb.Server do
  @moduledoc "Server responsible for managing a single shadow database."

  use GenServer
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Updates the data source definition."
  @spec update_definition(pid, map) :: :ok
  def update_definition(server, data_source), do: GenServer.cast(server, {:update, data_source})

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    Process.flag(:trap_exit, true)
    {:ok, %{data_source: nil}}
  end

  @impl GenServer
  def handle_cast({:update, data_source}, state) do
    if Application.get_env(:air, :shadow_db?, true) and state.data_source != data_source,
      do: update_shadow_db(data_source)

    {:noreply, %{state | data_source: data_source}}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_shadow_db(data_source) do
    Logger.info("data source definition changed for #{data_source.name}, updating shadow database")
    recreate_db!(data_source)
    create_tables!(data_source)
  end

  defp recreate_db!(data_source) do
    {:ok, conn} = Postgrex.start_link(hostname: "127.0.0.1", username: "postgres", database: "postgres")

    try do
      Postgrex.query!(conn, ~s/DROP DATABASE IF EXISTS "#{db_name(data_source.name)}"/, [])
      Postgrex.query!(conn, ~s/CREATE DATABASE "#{db_name(data_source.name)}"/, [])
    after
      close_conn(conn)
    end
  end

  defp create_tables!(data_source) do
    {:ok, conn} = Postgrex.start_link(hostname: "127.0.0.1", username: "postgres", database: db_name(data_source.name))

    try do
      data_source.tables
      |> Stream.reject(&Enum.empty?(Map.get(&1, :columns, [])))
      |> Stream.map(&{&1, Postgrex.query(conn, table_sql(&1), [])})
      |> Stream.filter(&match?({_table, {:error, _}}, &1))
      |> Enum.each(&report_error/1)
    after
      close_conn(conn)
    end
  end

  defp report_error({table, {:error, error}}),
    do: Logger.error("error creating shadow table #{table.id}: #{inspect(error)}")

  defp table_sql(table), do: ~s/CREATE TABLE "#{sanitize_name(to_string(table.id))}" (#{columns_sql(table.columns)})/

  defp columns_sql(columns), do: columns |> Enum.map(&column_sql/1) |> Enum.join(", ")

  defp column_sql(column), do: ~s/"#{sanitize_name(column.name)}" #{type_sql(column.type)}/

  defp type_sql(:boolean), do: "boolean"
  defp type_sql(:real), do: "real"
  defp type_sql(:integer), do: "integer"
  defp type_sql(:text), do: "text"
  defp type_sql(:date), do: "date"
  defp type_sql(:time), do: "time without time zone"
  defp type_sql(:datetime), do: "timestamp without time zone"
  defp type_sql(:unknown), do: "text"

  def db_name(data_source_name), do: "aircloak_shadow_#{sanitize_name(data_source_name)}"

  defp sanitize_name(name), do: Regex.replace(~r/"/, name, ~s/""/)

  defp close_conn(conn) do
    Process.exit(conn, :shutdown)

    receive do
      {:EXIT, ^conn, _reason} -> :ok
    after
      :timer.seconds(5) ->
        Process.exit(conn, :kill)

        receive do
          {:EXIT, ^conn, _reason} -> :ok
        end
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(registered_name), do: GenServer.start_link(__MODULE__, nil, name: registered_name)
end
