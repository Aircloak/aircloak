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
    {:ok, %{data_source_name: data_source_name, tables: []}}
  end

  @impl GenServer
  def handle_cast(:update_definition, state) do
    case Air.Service.DataSource.by_name(state.data_source_name) do
      nil ->
        {:stop, :normal, state}

      data_source ->
        tables = Air.Schemas.DataSource.tables(data_source)

        if Application.get_env(:air, :shadow_db?, true) and state.tables != tables,
          do: update_shadow_db(state, tables)

        {:noreply, %{state | tables: tables}}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_shadow_db(state, tables) do
    Logger.info("data source definition changed for #{state.data_source_name}, updating shadow database")
    ensure_db!(state.data_source_name)
    update_tables_definition(state, tables)
  end

  defp ensure_db!(data_source_name) do
    {:ok, conn} = Postgrex.start_link(hostname: "127.0.0.1", username: "postgres", database: "postgres")

    try do
      if match?(
           %Postgrex.Result{rows: [[0]]},
           Postgrex.query!(conn, "SELECT count(*) FROM pg_database where datname = $1", [db_name(data_source_name)])
         ) do
        Postgrex.query!(conn, ~s/CREATE DATABASE "#{sanitize_name(db_name(data_source_name))}"/, [])
      end
    after
      close_conn(conn)
    end
  end

  defp update_tables_definition(state, tables) do
    {:ok, conn} =
      Postgrex.start_link(hostname: "127.0.0.1", username: "postgres", database: db_name(state.data_source_name))

    tables = Enum.reject(tables, &Enum.empty?(Map.get(&1, "columns", [])))

    delete_obsolete_tables(conn, tables)

    try do
      tables
      |> changed_tables(state)
      |> Stream.map(&{&1, update_table_definition(conn, &1)})
      |> Stream.filter(&match?({_table, {:error, _}}, &1))
      |> Enum.each(&report_error/1)
    after
      close_conn(conn)
    end
  end

  defp delete_obsolete_tables(conn, tables) do
    known_tables = Enum.map(tables, &Map.fetch!(&1, "id"))

    conn
    |> Postgrex.query!("SELECT table_name FROM information_schema.tables where table_schema=$1", ["public"])
    |> Map.fetch!(:rows)
    |> Stream.map(fn [table_name] -> table_name end)
    |> Stream.reject(&Enum.member?(known_tables, &1))
    |> Enum.each(&Postgrex.query(conn, ~s/DROP TABLE "#{sanitize_name(&1)}"/, []))
  end

  defp changed_tables(tables, state) do
    tables
    |> Stream.map(&{&1, Enum.find(state.tables, fn table -> Map.fetch!(table, "id") == Map.fetch!(&1, "id") end)})
    |> Stream.reject(fn {table_def, prev_table_def} -> table_def == prev_table_def end)
    |> Enum.map(fn {table_def, _prev_table_def} -> table_def end)
  end

  defp update_table_definition(conn, table) do
    Postgrex.query(conn, ~s/DROP TABLE "#{sanitize_name(Map.fetch!(table, "id"))}"/, [])

    Postgrex.query(
      conn,
      ~s/CREATE TABLE "#{sanitize_name(Map.fetch!(table, "id"))}" (#{columns_sql(Map.fetch!(table, "columns"))})/,
      []
    )
  end

  defp report_error({table, {:error, error}}),
    do: Logger.error("error creating shadow table #{Map.fetch!(table, "id")}: #{inspect(error)}")

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

  defp db_name(data_source_name), do: "aircloak_shadow_#{data_source_name}"

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
