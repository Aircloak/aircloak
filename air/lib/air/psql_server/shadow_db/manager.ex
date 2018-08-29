defmodule Air.PsqlServer.ShadowDb.Manager do
  @moduledoc "Server responsible for managing a single shadow database."

  use GenServer, restart: :transient
  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the pid of the server related to the given data source, or `nil` if such server doesn't exist."
  @spec whereis(String.t()) :: pid | nil
  def whereis(data_source_name), do: GenServer.whereis(name(data_source_name))

  @doc "Updates the data source definition."
  @spec update_definition(String.t()) :: :ok
  def update_definition(data_source_name), do: GenServer.cast(name(data_source_name), :update_definition)

  @doc "Drops the given shadow database."
  @spec drop_database(String.t()) :: :ok
  def drop_database(data_source_name) do
    if Application.get_env(:air, :shadow_db?, true) do
      conn = connect!(Air.PsqlServer.ShadowDb.connection_params().name)

      try do
        # force close all existing connections to the database
        Postgrex.query(
          conn,
          """
          SELECT pg_terminate_backend(pg_stat_activity.pid)
          FROM pg_stat_activity
          WHERE pg_stat_activity.datname = $1 AND pid <> pg_backend_pid();
          """,
          [db_name(data_source_name)]
        )

        Postgrex.query(conn, ~s/DROP DATABASE IF EXISTS "#{sanitize_name(db_name(data_source_name))}"/, [])
      after
        close_conn(conn)
      end
    end

    :ok
  end

  @doc "Returns true if shadow db server is available, false otherwise."
  @spec db_server_available?() :: boolean
  def db_server_available?() do
    Task.async(fn -> connect!(Air.PsqlServer.ShadowDb.connection_params().name) end)
    |> Task.yield()
    |> case do
      {:ok, _pid} -> true
      _ -> false
    end
  end

  @doc "Returns the name of the shadow database for the given data source."
  @spec db_name(String.t()) :: String.t()
  def db_name(data_source_name), do: "aircloak_shadow_#{data_source_name}"

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(data_source_name) do
    Process.flag(:trap_exit, true)
    update_definition(data_source_name)
    {:ok, %{data_source_name: data_source_name, tables: []}}
  end

  @impl GenServer
  def handle_cast(:update_definition, state) do
    if Application.get_env(:air, :shadow_db?, true) do
      tables = data_source_tables(state.data_source_name)
      if state.tables != tables, do: update_shadow_db(state, tables)
      {:noreply, %{state | tables: tables}}
    else
      {:noreply, state}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(data_source_name), do: Air.PsqlServer.ShadowDb.registered_name(data_source_name, __MODULE__)

  defp connect!(database_name) do
    connection_params = Air.PsqlServer.ShadowDb.connection_params()

    {:ok, pid} =
      Postgrex.start_link(
        hostname: connection_params.host,
        port: connection_params.port,
        ssl: connection_params.ssl,
        username: connection_params.user,
        password: connection_params.password,
        database: database_name,
        sync_connect: true,
        backoff_type: :stop
      )

    pid
  end

  defp data_source_tables(data_source_name) do
    case Air.Service.DataSource.by_name(data_source_name) do
      nil -> []
      data_source -> data_source |> Air.Schemas.DataSource.tables() |> normalize_tables()
    end
  end

  defp normalize_tables(tables) do
    tables
    |> Stream.map(&normalize_table/1)
    |> Enum.reject(&Enum.empty?(&1.columns))
  end

  defp normalize_table(table) do
    %{
      id: Map.fetch!(table, "id"),
      columns: table |> Map.get("columns", []) |> Enum.map(&normalize_column/1)
    }
  end

  defp normalize_column(%{"name" => name, "type" => type}), do: %{name: name, type: type}

  defp update_shadow_db(state, tables) do
    Logger.info("data source definition changed for #{state.data_source_name}, updating shadow database")
    ensure_db!(state.data_source_name)
    update_tables_definition(state, tables)
  end

  defp ensure_db!(data_source_name) do
    conn = connect!(Air.PsqlServer.ShadowDb.connection_params().name)

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
    conn = connect!(db_name(state.data_source_name))

    try do
      delete_obsolete_tables(conn, tables)

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
    known_tables = Enum.map(tables, & &1.id)

    conn
    |> Postgrex.query!("SELECT table_name FROM information_schema.tables where table_schema=$1", ["public"])
    |> Map.fetch!(:rows)
    |> Stream.map(fn [table_name] -> table_name end)
    |> Stream.reject(&Enum.member?(known_tables, &1))
    |> Enum.each(&Postgrex.query(conn, ~s/DROP TABLE "#{sanitize_name(&1)}"/, []))
  end

  defp changed_tables(tables, state) do
    tables
    |> Stream.map(&{&1, Enum.find(state.tables, fn table -> table.id == &1.id end)})
    |> Stream.reject(fn {table_def, prev_table_def} -> table_def == prev_table_def end)
    |> Enum.map(fn {table_def, _prev_table_def} -> table_def end)
  end

  defp update_table_definition(conn, table) do
    Postgrex.query(conn, ~s/DROP TABLE "#{sanitize_name(table.id)}"/, [])

    Postgrex.query(
      conn,
      ~s/CREATE TABLE "#{sanitize_name(table.id)}" (#{columns_sql(table.columns)})/,
      []
    )
  end

  defp report_error({table, {:error, error}}),
    do: Logger.error("error creating shadow table #{table.id}: #{inspect(error)}")

  defp columns_sql(columns), do: columns |> Enum.map(&column_sql/1) |> Enum.join(", ")

  defp column_sql(column), do: ~s/"#{sanitize_name(column.name)}" #{type_sql(column.type)}/

  defp type_sql("boolean"), do: "boolean"
  defp type_sql("real"), do: "real"
  defp type_sql("integer"), do: "integer"
  defp type_sql("text"), do: "text"
  defp type_sql("date"), do: "date"
  defp type_sql("time"), do: "time without time zone"
  defp type_sql("datetime"), do: "timestamp without time zone"
  defp type_sql("unknown"), do: "text"

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
  def start_link(data_source_name), do: GenServer.start_link(__MODULE__, data_source_name, name: name(data_source_name))
end
