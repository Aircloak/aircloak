defmodule Cloak.DataSource.Driver.RodbcSql do
  @moduledoc "Common boilerplate for SQL drivers powered by RODBC."
  alias Cloak.DataSource.{Driver, RODBC, SqlBuilder}

  defmacro __using__(_opts) do
    quote do
      use Driver.SQL

      @impl Driver
      defdelegate disconnect(connection), to: RODBC

      @impl Driver
      defdelegate load_tables(connection, table), to: RODBC

      @impl Driver
      def select(connection, query, result_processor),
        do: RODBC.select(connection, SqlBuilder.build(query), query.db_columns, result_processor)

      @impl Driver
      defdelegate driver_info(connection), to: RODBC

      @impl Driver.SQL
      defdelegate execute(connection, sql), to: RODBC, as: :execute_direct

      @impl Driver.SQL
      def select(connection, sql), do: execute(connection, sql)

      defoverridable Driver
      defoverridable Driver.SQL
    end
  end
end
