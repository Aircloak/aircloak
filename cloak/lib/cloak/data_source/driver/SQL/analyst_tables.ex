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
      def create_or_update_analyst_table(connection, key, db_name, sql, statement, fingerprint) do
        AnalystTables.create_or_update_analyst_table(
          __MODULE__,
          connection,
          key,
          db_name,
          sql,
          statement,
          fingerprint
        )
      end

      @impl Driver
      def drop_analyst_table(connection, db_name), do: AnalystTables.drop_analyst_table(__MODULE__, connection, db_name)

      @impl Driver
      def registered_analyst_tables(connection), do: AnalystTables.registered_analyst_tables(__MODULE__, connection)

      @impl AnalystTables
      def analyst_tables(connection), do: AnalystTables.analyst_tables(__MODULE__, connection)

      defoverridable Driver
      defoverridable AnalystTables
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @analyst_meta_table_name "__ac_analyst_tables_1"

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
  def create_or_update_analyst_table(driver, connection, key, db_name, sql, statement, fingerprint) do
    with {:ok, _} <- drop_table(driver, connection, key, db_name),
         {:ok, _} <- create_table(driver, connection, key, db_name, sql, statement, fingerprint),
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

  @doc false
  def registered_analyst_tables(driver, connection) do
    columns = ~w(key statement fingerprint name) |> Stream.map(&quote_identifier(driver, &1)) |> Enum.join(", ")

    connection
    |> driver.select!("SELECT #{columns} FROM #{quoted_analyst_table_name(driver)}")
    |> Enum.map(fn [key, statement, fingerprint, name] ->
      %{key: key, statement: Base.decode64!(statement), fingerprint: Base.decode64!(fingerprint), db_name: name}
    end)
  end

  defp drop_table(driver, connection, key, db_name) do
    # dropping previous table will make sure that if db_name algorithm is changed, we don't keep the old tables
    drop_previous_table(driver, connection, key)

    # we're also dropping the table with the pending name, to clear a possibly non-registered table (created with the
    # previous version of the cloak)
    driver.execute(connection, "DROP TABLE #{quote_identifier(driver, db_name)}")

    delete_meta(driver, connection, key)
  end

  defp drop_previous_table(driver, connection, key) do
    driver.select!(
      connection,
      """
      SELECT #{quote_identifier(driver, "name")} FROM #{quoted_analyst_table_name(driver)}
      WHERE #{quote_identifier(driver, "key")} = '#{SqlBuilder.escape_string(key)}'
      """
    )
    |> Enum.to_list()
    |> case do
      [[name]] -> driver.execute(connection, "DROP TABLE #{quote_identifier(driver, name)}")
      [] -> nil
    end
  end

  defp delete_meta(driver, connection, key) do
    driver.execute(
      connection,
      """
      DELETE FROM #{quoted_analyst_table_name(driver)}
      WHERE #{quote_identifier(driver, "key")} = '#{SqlBuilder.escape_string(key)}'
      """
    )
  end

  defp create_table(driver, connection, key, db_name, sql, statement, fingerprint) do
    Logger.info("creating database table `#{db_name}`")

    with {:ok, _} <- insert_meta(driver, connection, key, db_name, statement, fingerprint),
         do: driver.execute(connection, sql)
  end

  defp insert_meta(driver, connection, key, db_name, statement, fingerprint) do
    columns = ~w(key name statement fingerprint) |> Stream.map(&quote_identifier(driver, &1)) |> Enum.join(", ")

    # Note: we're encoding the statement with base64 to avoid possible encoding issues on the underlying database.
    # Base64 should ensure that we can get the exact same statement that was originally submitted to create the table.
    values =
      [key, db_name, Base.encode64(statement), Base.encode64(fingerprint)]
      |> Stream.map(&"'#{SqlBuilder.escape_string(&1)}'")
      |> Enum.join(", ")

    driver.execute(connection, "INSERT INTO #{quoted_analyst_table_name(driver)} (#{columns}) VALUES (#{values})")
  end

  defp quoted_analyst_table_name(driver), do: quote_identifier(driver, @analyst_meta_table_name)

  defp quote_identifier(driver, identifier),
    do: SqlBuilder.quote_table_name(identifier, driver.sql_dialect_module().quote_char())
end
