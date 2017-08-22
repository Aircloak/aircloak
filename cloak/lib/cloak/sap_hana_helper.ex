defmodule SapHanaHelper do
  @moduledoc "Helper functions for working with SAP HANA database."
  @type conn :: :odbc.connection_reference


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Connects to the database."
  @spec connect(String.t, integer, String.t, String.t, String.t) :: {:ok, conn} | {:error, any}
  def connect(host, port, user, password, database) do
    [
      driver: "#{Application.app_dir(:cloak, "priv/odbc/drivers")}/libodbc-sap-hana-v2.so",
      servernode: "#{host}:#{port}",
      uid: user,
      pwd: password,
      databasename: database
    ]
    |> Enum.map(fn({key, value}) -> [to_string(key), ?=, value] end)
    |> Enum.join(";")
    |> to_char_list()
    |> :odbc.connect(auto_commit: :on, binary_strings: :on, tuple_row: :off)
  end

  @doc "Executes the database query."
  @spec execute(conn, String.t, [any]) :: :odbc.result_tuple
  def execute(conn, command, params \\ []), do:
    :odbc.param_query(conn, to_char_list(command), params)

  @doc "Executes the select query."
  @spec select!(conn, String.t, [any]) :: [[any]]
  def select!(conn, command, params \\ []) do
    {:selected, _fields, rows} = execute(conn, command, params)
    rows
  end

  @doc "Creates the database schema if it doesn't exist."
  @spec ensure_schema!(conn, String.t) :: :ok
  def ensure_schema!(conn, schema_name) do
    case select!(conn, "select schema_name from schemas where schema_name='#{schema_name}'") do
      [_] -> :ok
      [] ->
        {:updated, _} = execute(conn, "create schema #{schema_name}")
        :ok
    end
  end

  @doc "Drops the schema and all the objects it contains."
  @spec drop_schema_cascade!(conn, String.t) :: :ok
  def drop_schema_cascade!(conn, schema_name) do
    {:updated, _} = execute(conn, "drop schema #{schema_name} CASCADE")
    :ok
  end

  @doc "Recreates the table according to the provided definition."
  @spec recreate_table!(conn, String.t, String.t, String.t) :: :ok
  def recreate_table!(conn, schema_name, table_name, table_def) do
    if table_exists?(conn, schema_name, table_name), do:
      {:updated, _} = execute(conn, "DROP TABLE #{schema_name}.#{table_name}")

    {:updated, _} = execute(conn, "CREATE TABLE #{schema_name}.#{table_name} (#{table_def})")
    :ok
  end

  @doc "Inserts multiple rows into the database table."
  @spec insert_rows!(conn, String.t, String.t, [String.t], [[any]]) :: :ok
  def insert_rows!(conn, schema_name, table_name, columns, rows) do
    rows
    |> Stream.map(&'SELECT #{Enum.join(&1, ", ")} from dummy')
    |> Stream.chunk(1000, 1000, [])
    |> Stream.map(&'(#{Enum.join(&1, " UNION ALL ")})')
    |> Enum.each(fn(chunk_sql) ->
      {:updated, _} =
        execute(
          conn,
          "INSERT INTO #{schema_name}.#{table_name}(#{Enum.join(columns, ", ")}) #{chunk_sql}"
        )
    end)

    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp table_exists?(conn, schema_name, table_name), do:
    match?(
      [_],
      select!(
        conn,
        "SELECT table_name FROM tables WHERE table_name='#{table_name}' AND schema_name='#{schema_name}'"
      )
    )
end
