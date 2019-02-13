defmodule Cloak.DataSource.Driver.SQL do
  @moduledoc "Helper module for implementing SQL drivers."

  require Logger
  alias Cloak.DataSource.Driver
  alias Cloak.DataSource.SqlBuilder

  @doc "Executes the given SQL statement."
  @callback execute(Driver.connection(), String.t()) :: {:ok, any} | {:error, String.t()}

  @doc "Executes the given select statement and returns an enumerable of rows, where each row should be a list."
  @callback select(Driver.connection(), String.t()) :: {:ok, Enumerable.t()} | {:error, String.t()}

  @doc "Returns the list of analyst tables."
  @callback analyst_tables(Driver.connection()) :: [String.t()]

  defmacro __using__(_opts) do
    module = __CALLER__.module |> Module.split() |> List.last()
    dialect = Module.concat(SqlBuilder, module)

    quote do
      alias Cloak.DataSource.{Driver, SqlBuilder}
      alias Driver.SQL
      use Driver
      require Logger

      @behaviour unquote(__MODULE__)

      @impl unquote(__MODULE__)
      def analyst_tables(connection) do
        connection
        # using apply here to trick the dialyzer which will report errors for data sources which don't implement
        # select_table_names function in the dialect
        |> select!(apply(__MODULE__, :sql_dialect_module, []).select_table_names("__ac_"))
        |> Stream.map(fn [table_name] -> table_name end)
        |> Enum.reject(&(&1 == SQL.unquoted_analyst_table_name()))
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
      def prepare_analyst_table(db_name, query), do: SQL.create_table_from_query(db_name, query)

      @impl Driver
      def create_or_update_analyst_table(connection, db_name, sql, air_id, data_source_name),
        do: SQL.create_or_update_analyst_table(__MODULE__, connection, db_name, sql, air_id, data_source_name)

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

      @impl Driver
      def initialize_analyst_meta_table(connection) do
        # using apply here to trick the dialyzer which will report errors for data sources which don't implement
        # sql_dialect_module function in the dialect
        sql = apply(__MODULE__, :sql_dialect_module, []).select_table_names(SQL.unquoted_analyst_table_name())

        if Enum.empty?(select!(connection, sql)) do
          quoted_table_name = SQL.quoted_analyst_table_name(unquote(dialect))
          # using apply here to trick the dialyzer which will report errors for data sources which don't implement
          # analyst_meta_table_create_statement function in the dialect
          sql = apply(unquote(dialect), :analyst_meta_table_create_statement, [quoted_table_name])
          with {:ok, _} <- execute(connection, sql), do: :ok
        else
          # the table already exists, so we won't do anything else
          :ok
        end
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

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the unquoted analyst table name."
  @spec unquoted_analyst_table_name() :: String.t()
  def unquoted_analyst_table_name(), do: "__ac_analyst_tables"

  @doc "Returns the quoted analyst table name."
  @spec quoted_analyst_table_name(module) :: String.t()
  def quoted_analyst_table_name(dialect), do: quote_identifier(dialect, unquoted_analyst_table_name())

  @doc "Returns the SQL statement for creating the table populated with the given query."
  @spec create_table_from_query(String.t(), Query.t()) :: String.t()
  def create_table_from_query(table_name, query) do
    quoted_table_name =
      SqlBuilder.quote_table_name(table_name, query.data_source.driver.sql_dialect_module.quote_char())

    select_statement = SqlBuilder.build(query)
    "CREATE TABLE #{quoted_table_name} AS #{select_statement}"
  end

  @doc "Creates or updates the given analyst table."
  @spec create_or_update_analyst_table(module, Driver.connection(), String.t(), String.t(), String.t(), String.t()) ::
          :ok | {:error, String.t()}
  def create_or_update_analyst_table(driver, connection, db_name, sql, air_id, data_source_name) do
    with {:ok, _} <- drop_table(driver, connection, db_name, air_id, data_source_name),
         {:ok, _} <- create_table(driver, connection, db_name, sql, air_id, data_source_name),
         do: :ok
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp drop_table(driver, connection, db_name, air_id, data_source_name) do
    if Enum.any?(driver.analyst_tables(connection), &(&1 == db_name)) do
      Logger.info("dropping database table `#{db_name}` because it will be recreated")

      with {:ok, _} <- delete_meta(driver, connection, air_id, data_source_name, db_name),
           do: driver.execute(connection, "DROP TABLE #{quote_identifier(driver.sql_dialect_module(), db_name)}")
    else
      {:ok, nil}
    end
  end

  defp delete_meta(driver, connection, air_id, data_source_name, db_name) do
    filter =
      [{"air", air_id}, {"data_source", data_source_name}, {"name", db_name}]
      |> Stream.map(fn {column, value} ->
        "#{quote_identifier(driver.sql_dialect_module(), column)} = '#{SqlBuilder.escape_string(value)}'"
      end)
      |> Enum.join(" AND ")

    driver.execute(connection, "DELETE FROM #{quoted_analyst_table_name(driver.sql_dialect_module())} WHERE #{filter}")
  end

  defp create_table(driver, connection, db_name, sql, air_id, data_source_name) do
    Logger.info("creating database table `#{db_name}`")

    with {:ok, _} <- insert_meta(driver, connection, air_id, data_source_name, db_name),
         do: driver.execute(connection, sql)
  end

  defp insert_meta(driver, connection, air_id, data_source_name, db_name) do
    {columns, values} =
      [{"air", air_id}, {"data_source", data_source_name}, {"name", db_name}]
      |> Stream.map(fn {column, value} ->
        {quote_identifier(driver.sql_dialect_module(), column), "'#{SqlBuilder.escape_string(value)}'"}
      end)
      |> Enum.unzip()

    columns = Enum.join(columns, ", ")
    values = Enum.join(values, ", ")

    statement = "INSERT INTO #{quoted_analyst_table_name(driver.sql_dialect_module())} (#{columns}) VALUES (#{values})"
    driver.execute(connection, statement)
  end

  defp quote_identifier(dialect, identifier), do: SqlBuilder.quote_table_name(identifier, dialect.quote_char())
end
