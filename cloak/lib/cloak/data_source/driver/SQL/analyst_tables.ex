defmodule Cloak.DataSource.Driver.SQL.AnalystTables do
  @moduledoc "Common implementation of analyst tables functionality for SQL based drivers."

  require Logger
  alias Cloak.DataSource.{Driver, SqlBuilder}
  alias __MODULE__

  @doc "Returns the list of analyst tables."
  @callback analyst_tables(Driver.connection()) :: [String.t()]

  defmacro __using__(_opts) do
    quote do
      @behaviour AnalystTables

      @impl Driver
      def initialize_analyst_meta_table(connection),
        do: AnalystTables.initialize_analyst_meta_table(__MODULE__, connection)

      @impl Driver
      def prepare_analyst_table(db_name, query), do: AnalystTables.create_table_from_query(db_name, query)

      @impl Driver
      def create_or_update_analyst_table(connection, db_name, sql, air_id, data_source_name),
        do: AnalystTables.create_or_update_analyst_table(__MODULE__, connection, db_name, sql, air_id, data_source_name)

      @impl Driver
      def drop_analyst_table(connection, db_name), do: AnalystTables.drop_analyst_table(__MODULE__, connection, db_name)

      @impl AnalystTables
      def analyst_tables(connection), do: AnalystTables.analyst_tables(__MODULE__, connection)

      defoverridable Driver
      defoverridable AnalystTables
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @analyst_meta_table_name "__ac_analyst_tables"

  @doc false
  def initialize_analyst_meta_table(driver, connection) do
    sql = driver.sql_dialect_module().select_table_names(@analyst_meta_table_name)

    if Enum.empty?(driver.select!(connection, sql)) do
      quoted_table_name = quoted_analyst_table_name(driver)
      sql = driver.sql_dialect_module().analyst_meta_table_create_statement(quoted_table_name)
      with {:ok, _} <- driver.execute(connection, sql), do: :ok
    else
      # the table already exists, so we won't do anything else
      :ok
    end
  end

  @doc false
  def create_table_from_query(table_name, query) do
    quoted_table_name =
      SqlBuilder.quote_table_name(table_name, query.data_source.driver.sql_dialect_module.quote_char())

    select_statement = SqlBuilder.build(query)
    "CREATE TABLE #{quoted_table_name} AS #{select_statement}"
  end

  @doc false
  def create_or_update_analyst_table(driver, connection, db_name, sql, air_id, data_source_name) do
    with {:ok, _} <- drop_table(driver, connection, db_name, air_id, data_source_name),
         {:ok, _} <- create_table(driver, connection, db_name, sql, air_id, data_source_name),
         do: :ok
  end

  def drop_analyst_table(driver, connection, db_name) do
    Logger.info("dropping analyst table #{db_name}")

    with {:ok, _} <-
           driver.execute(
             connection,
             """
             DELETE FROM #{quoted_analyst_table_name(driver)}
             WHERE #{quote_identifier(driver, "name")} = '#{SqlBuilder.escape_string(db_name)}'
             """
           ) do
      driver.execute(connection, "DROP TABLE #{quote_identifier(driver, db_name)}")

      # Note that we don't return an error if a table has not been dropped successfully. At this point, the
      # table has already been removed from the meta table, so we'll consider the operation to be successful.
      :ok
    end
  end

  @doc false
  def analyst_tables(driver, connection) do
    connection
    |> driver.select!(driver.sql_dialect_module().select_table_names("__ac_"))
    |> Stream.map(fn [table_name] -> table_name end)
    |> Enum.reject(&(&1 == @analyst_meta_table_name))
  end

  defp drop_table(driver, connection, db_name, air_id, data_source_name) do
    if Enum.any?(driver.analyst_tables(connection), &(&1 == db_name)) do
      Logger.info("dropping database table `#{db_name}` because it will be recreated")

      with {:ok, _} <- delete_meta(driver, connection, air_id, data_source_name, db_name),
           do: driver.execute(connection, "DROP TABLE #{quote_identifier(driver, db_name)}")
    else
      {:ok, nil}
    end
  end

  defp delete_meta(driver, connection, air_id, data_source_name, db_name) do
    filter =
      [{"air", air_id}, {"data_source", data_source_name}, {"name", db_name}]
      |> Stream.map(fn {column, value} ->
        "#{quote_identifier(driver, column)} = '#{SqlBuilder.escape_string(value)}'"
      end)
      |> Enum.join(" AND ")

    driver.execute(connection, "DELETE FROM #{quoted_analyst_table_name(driver)} WHERE #{filter}")
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
        {quote_identifier(driver, column), "'#{SqlBuilder.escape_string(value)}'"}
      end)
      |> Enum.unzip()

    columns = Enum.join(columns, ", ")
    values = Enum.join(values, ", ")

    driver.execute(connection, "INSERT INTO #{quoted_analyst_table_name(driver)} (#{columns}) VALUES (#{values})")
  end

  defp quoted_analyst_table_name(driver), do: quote_identifier(driver, @analyst_meta_table_name)

  defp quote_identifier(driver, identifier),
    do: SqlBuilder.quote_table_name(identifier, driver.sql_dialect_module().quote_char())
end
