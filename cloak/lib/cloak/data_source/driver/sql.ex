defmodule Cloak.DataSource.Driver.SQL do
  @moduledoc "Helper module for implementing SQL drivers."

  alias Cloak.DataSource.SqlBuilder

  @doc "Executes the given SQL statement."
  @callback execute(Cloak.DataSource.Driver.connection(), String.t()) :: {:ok, any} | {:error, String.t()}

  @doc "Executes the given select statement and returns an enumerable of rows, where each row should be a list."
  @callback select(Cloak.DataSource.Driver.connection(), String.t()) :: {:ok, Enumerable.t()} | {:error, String.t()}

  @doc "Returns the list of analyst tables."
  @callback analyst_tables(Cloak.DataSource.Driver.connection()) :: [String.t()]

  defmacro __using__(_opts) do
    module = __CALLER__.module |> Module.split() |> List.last()
    dialect = Module.concat(SqlBuilder, module)

    quote do
      alias Cloak.DataSource.{Driver, SqlBuilder}
      use Driver

      @behaviour unquote(__MODULE__)

      @impl unquote(__MODULE__)
      def analyst_tables(connection) do
        connection
        # using apply here to trick the dialyzer which will report errors for data sources which don't implement
        # select_table_names function in the dialect
        |> select!(apply(__MODULE__, :sql_dialect_module, []).select_table_names("__ac_"))
        |> Enum.map(fn [table_name] -> table_name end)
      end

      @impl Driver
      def sql_dialect_module(), do: unquote(dialect)

      @impl Driver
      def disconnect(connection), do: GenServer.stop(connection, :normal, :timer.seconds(5))

      @impl Driver
      def supports_query?(_query), do: true

      @impl Driver
      defdelegate supports_function?(expression, data_source), to: SqlBuilder.Support

      @impl Driver
      def prepare_analyst_table(table_id, query) do
        {sql, db_name} = SqlBuilder.create_table_statement(table_id, query)
        {db_name, sql}
      end

      @impl Driver
      def store_analyst_table(connection, db_name, sql) do
        if Enum.any?(analyst_tables(connection), &(&1 == db_name)) do
          :ok
        else
          with {:ok, _} <- execute(connection, sql), do: :ok
        end
      end

      @impl Driver
      def drop_unused_analyst_tables(connection, known_db_names) do
        require Logger

        connection
        |> analyst_tables()
        |> MapSet.new()
        |> MapSet.difference(MapSet.new(known_db_names))
        |> Stream.map(fn db_name ->
          case execute(connection, "DROP TABLE #{SqlBuilder.quote_table_name(db_name)}") do
            {:ok, _result} -> db_name
            {:error, error} -> Logger.error("Error removing table: `#{db_name}`: #{error}")
          end
        end)
        |> Enum.reject(&is_nil/1)
      end

      @doc false
      def execute!(connection, sql) do
        case execute(connection, sql) do
          {:ok, result} -> result
          {:error, reason} -> raise(RuntimeError, reason)
        end
      end

      @doc false
      def select!(connection, sql) do
        case select(connection, sql) do
          {:ok, result} -> result
          {:error, reason} -> raise(RuntimeError, reason)
        end
      end

      defoverridable Driver
      defoverridable unquote(__MODULE__)
    end
  end
end
