defmodule Cloak.DataSource.Driver.SQL do
  @moduledoc "Helper module for implementing SQL drivers."

  alias Cloak.DataSource.SqlBuilder

  defmacro __using__(_opts) do
    module = __CALLER__.module |> Module.split() |> List.last()
    dialect = Module.concat(SqlBuilder, module)

    quote do
      alias Cloak.DataSource.{Driver, SqlBuilder}

      @behaviour Driver

      @impl Driver
      def sql_dialect_module(), do: unquote(dialect)

      @impl Driver
      def disconnect(connection), do: GenServer.stop(connection, :normal, :timer.seconds(5))

      @impl Driver
      def supports_query?(_query), do: true

      @impl Driver
      defdelegate supports_function?(expression, data_source), to: SqlBuilder.Support

      defoverridable sql_dialect_module: 0,
                     disconnect: 1,
                     supports_query?: 1,
                     supports_function?: 2
    end
  end
end
