defmodule Cloak.DataSource.Driver.SQL do
  @moduledoc "Helper module for implementing SQL drivers."

  require Logger
  alias Cloak.DataSource.Driver
  alias Cloak.DataSource.SqlBuilder

  @doc "Executes the given SQL statement."
  @callback execute(Driver.connection(), String.t()) :: {:ok, any} | {:error, String.t()}

  @doc "Executes the given select statement and returns an enumerable of rows, where each row should be a list."
  @callback select(Driver.connection(), String.t()) :: {:ok, Enumerable.t()} | {:error, String.t()}

  defmacro __using__(_opts) do
    module = __CALLER__.module |> Module.split() |> List.last()
    dialect = Module.concat(SqlBuilder, module)

    quote do
      alias Cloak.DataSource.{Driver, SqlBuilder}
      use Driver

      @behaviour unquote(__MODULE__)

      @impl Driver
      def sql_dialect_module(), do: unquote(dialect)

      @impl Driver
      def disconnect(connection), do: GenServer.stop(connection, :normal, :timer.seconds(5))

      @impl Driver
      def supports_query?(_query), do: true

      @impl Driver
      defdelegate supports_function?(expression, data_source), to: SqlBuilder.Support

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
