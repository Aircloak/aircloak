defmodule Cloak.SapHanaHelpers do
  if Mix.env() in [:dev, :test] do
    @moduledoc "Helper functions for working with SAP HANA database."
    @type conn :: :odbc.connection_reference
    @type connection_params :: %{
      hostname: String.t,
      port: pos_integer,
      username: String.t,
      password: String.t,
      database: String.t,
      default_schema: String.t,
    }


    # -------------------------------------------------------------------
    # API functions
    # -------------------------------------------------------------------

    @doc "Connects to the database."
    @spec connect(connection_params) :: {:ok, conn} | {:error, any}
    def connect(connection_params) do
      [
        servernode: "#{connection_params.hostname}:#{connection_params.port}",
        uid: connection_params.username,
        pwd: connection_params.password,
        databasename: connection_params.database,
        cs:
          case Map.fetch(connection_params, :default_schema) do
            {:ok, schema} -> ~s/"#{schema}"/
            :error -> nil
          end
      ]
      |> Enum.reject(&match?({_key, nil}, &1))
      |> Keyword.merge(driver_option())
      |> Enum.map(fn({key, value}) -> [to_string(key), ?=, value] end)
      |> Enum.join(";")
      |> to_charlist()
      |> :odbc.connect(auto_commit: :on, binary_strings: :on, tuple_row: :off)
    end

    @doc "Executes the database query."
    @spec execute(conn, String.t, [any]) :: :odbc.result_tuple
    def execute(conn, command, params \\ []), do:
      :odbc.param_query(conn, to_charlist(command), params)

    @doc "Executes the database query. Raises on error."
    @spec execute!(conn, String.t, [any]) :: :ok
    def execute!(conn, command, params \\ []) do
      conn
      |> execute(command, params)
      |> case do
        {:updated, _} -> :ok
        {:error, error} ->
          raise to_string(error)
      end
    end

    @doc "Executes the select query."
    @spec select!(conn, String.t, [any]) :: [[any]]
    def select!(conn, command, params \\ []) do
      {:selected, _fields, rows} = execute(conn, command, params)
      rows
    end

    @doc "Creates the desired schema if it doesn't exist."
    @spec ensure_schema!(connection_params | pid, String.t) :: :ok
    def ensure_schema!(conn_or_connection_params, schema_name), do:
      :ok = ensure_schema(conn_or_connection_params, schema_name)

    @doc "Creates the desired schema if it doesn't exist."
    @spec ensure_schema(connection_params | pid, String.t) :: :ok | {:error, :reason}
    def ensure_schema(%{} = connection_params, schema_name) do
      with {:ok, conn} <- connect(Map.delete(connection_params, :default_schema)) do
        try do
          ensure_schema(conn, schema_name)
        after
          :odbc.disconnect(conn)
        end
      end
    end
    def ensure_schema(conn, schema_name) when is_pid(conn) do
      case select!(conn, "select schema_name from schemas where schema_name='#{schema_name}'") do
        [_] -> :ok
        [] ->
          case execute(conn, ~s/create schema "#{schema_name}"/) do
            {:updated, _} -> :ok
            _ -> {:error, :schema_create}
          end
      end
    end

    @doc "Recreates the table according to the provided definition."
    @spec recreate_table!(conn, String.t, String.t, String.t) :: :ok
    def recreate_table!(conn, schema_name, table_name, table_def) do
      if table_exists?(conn, schema_name, table_name), do:
        execute!(conn, ~s/DROP TABLE "#{schema_name}"."#{table_name}"/)

      execute!(conn, ~s/CREATE TABLE "#{schema_name}"."#{table_name}" (#{table_def})/)
      :ok
    end

    @doc "Inserts multiple rows into the database table."
    @spec insert_rows!(conn, String.t, String.t, [String.t], [[any]]) :: :ok
    def insert_rows!(conn, schema_name, table_name, columns, rows) do
      quoted_column_names = columns |> Enum.map(&~s/"#{&1}"/) |>  Enum.join(", ")
      value_placeholders = List.duplicate("?", length(columns)) |> Enum.join(", ")
      row_placeholder = "SELECT #{value_placeholders} FROM dummy"

      rows
      |> Stream.chunk(100, 100, [])
      |> Stream.map(fn (chunk) ->
        select_sql = List.duplicate(row_placeholder, length(chunk)) |> Enum.join(" UNION ALL ")
        values = chunk |> List.flatten() |> Enum.map(&cast_types/1)
        {select_sql, values}
      end)
      |> Enum.each(fn ({sql, values}) ->
        execute!(conn, ~s/INSERT INTO "#{schema_name}"."#{table_name}"(#{quoted_column_names}) (#{sql})/, values)
      end)

      :ok
    end

    @doc "Deletes old test schemas from the database."
    @spec delete_test_schemas() :: :ok
    def delete_test_schemas() do
      # Note: checking for CI instead of TRAVIS env, to allow local `make test_all` to work.
      if System.get_env("CI") == "true" do
        {:ok, conn} =
          Application.fetch_env!(:cloak, :sap_hana)
          |> Map.new()
          |> Map.delete(:default_schema)
          |> connect()

        # we'll delete all test schemas older than 2 hours
        query =
          "
            select schema_name from schemas
              where lower(schema_name) like 'test_schema_%'
              and (seconds_between(create_time, now()) / 3600) > 2
          "

        conn
        |> select!(query)
        |> Enum.map(&:unicode.characters_to_binary(&1, {:utf16, :little}))
        |> Enum.each(&execute!(conn, ~s/drop schema "#{&1}" cascade/))
      end

      :ok
    end


    # -------------------------------------------------------------------
    # Internal functions
    # -------------------------------------------------------------------

    defp driver_option() do
      if System.get_env("TRAVIS") == "true" do
        [dsn: "SAPHANA"]
      else
        [driver: "#{Application.app_dir(:cloak, "priv/odbc/drivers")}/libodbc-sap-hana-v2.so"]
      end
    end

    defp table_exists?(conn, schema_name, table_name), do:
      match?(
        [_],
        select!(
          conn,
          "SELECT table_name FROM tables WHERE table_name='#{table_name}' AND schema_name='#{schema_name}'"
        )
      )

    defp cast_types(binary) when is_binary(binary) do
      binary = :unicode.characters_to_binary(binary, :utf8, {:utf16, :little})
      {{:sql_wvarchar, length_null_terminated(binary)}, [binary]}
    end
    defp cast_types(integer) when is_integer(integer), do: {:sql_integer, [integer]}
    defp cast_types(float) when is_float(float), do: {:sql_real, [float]}
    defp cast_types(boolean) when is_boolean(boolean), do: {:sql_bit, [boolean]}
    defp cast_types(%{calendar: Calendar.ISO} = datetime), do: datetime |> to_string() |> cast_types()

    defp length_null_terminated(binary), do: byte_size(binary) + 1
  end
end
