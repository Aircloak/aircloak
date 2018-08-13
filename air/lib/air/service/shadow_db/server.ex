defmodule Air.Service.ShadowDb.Server do
  @moduledoc "Server responsible for managing a single shadow database."

  use GenServer, restart: :transient
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Updates the data source definition."
  @spec update_definition(pid) :: :ok
  def update_definition(server), do: GenServer.cast(server, :update_definition)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(data_source_name) do
    Process.flag(:trap_exit, true)
    update_definition(self())
    {:ok, %{data_source_name: data_source_name, tables: nil}}
  end

  @impl GenServer
  def handle_cast(:update_definition, state) do
    case Air.Service.DataSource.by_name(state.data_source_name) do
      nil ->
        {:stop, :normal, state}

      data_source ->
        tables = Air.Schemas.DataSource.tables(data_source)

        if Application.get_env(:air, :shadow_db?, true) and state.tables != tables,
          do: update_shadow_db(state.data_source_name, tables)

        {:noreply, %{state | tables: tables}}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_shadow_db(data_source_name, tables) do
    Logger.info("data source definition changed for #{data_source_name}, updating shadow database")
    recreate_db!(data_source_name)
    create_tables!(data_source_name, tables)
  end

  defp recreate_db!(data_source_name) do
    {:ok, conn} = Postgrex.start_link(hostname: "127.0.0.1", username: "postgres", database: "postgres")

    try do
      Postgrex.query!(conn, ~s/DROP DATABASE IF EXISTS "#{db_name(data_source_name)}"/, [])
      Postgrex.query!(conn, ~s/CREATE DATABASE "#{db_name(data_source_name)}"/, [])
    after
      close_conn(conn)
    end
  end

  defp create_tables!(data_source_name, tables) do
    {:ok, conn} = Postgrex.start_link(hostname: "127.0.0.1", username: "postgres", database: db_name(data_source_name))

    try do
      tables
      |> Stream.reject(&Enum.empty?(Map.get(&1, "columns", [])))
      |> Stream.map(&{&1, Postgrex.query(conn, table_sql(&1), [])})
      |> Stream.filter(&match?({_table, {:error, _}}, &1))
      |> Enum.each(&report_error/1)
    after
      close_conn(conn)
    end
  end

  defp report_error({table, {:error, error}}),
    do: Logger.error("error creating shadow table #{Map.fetch!(table, "id")}: #{inspect(error)}")

  defp table_sql(table),
    do: ~s/CREATE TABLE "#{sanitize_name(Map.fetch!(table, "id"))}" (#{columns_sql(Map.fetch!(table, "columns"))})/

  defp columns_sql(columns), do: columns |> Enum.map(&column_sql/1) |> Enum.join(", ")

  defp column_sql(column),
    do: ~s/"#{sanitize_name(Map.fetch!(column, "name"))}" #{type_sql(Map.fetch!(column, "type"))}/

  defp type_sql("boolean"), do: "boolean"
  defp type_sql("real"), do: "real"
  defp type_sql("integer"), do: "integer"
  defp type_sql("text"), do: "text"
  defp type_sql("date"), do: "date"
  defp type_sql("time"), do: "time without time zone"
  defp type_sql("datetime"), do: "timestamp without time zone"
  defp type_sql("unknown"), do: "text"

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
  def start_link({data_source_name, registered_name}),
    do: GenServer.start_link(__MODULE__, data_source_name, name: registered_name)
end
