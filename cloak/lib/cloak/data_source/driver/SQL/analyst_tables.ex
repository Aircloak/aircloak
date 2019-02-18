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
      def create_or_update_analyst_table(connection, table, sql),
        do: AnalystTables.create_or_update_analyst_table(__MODULE__, connection, table, sql)

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
  def create_or_update_analyst_table(driver, connection, table, sql) do
    with {:ok, _} <- drop_table(driver, connection, table),
         {:ok, _} <- create_table(driver, connection, table, sql),
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
    existing_db_names = MapSet.new(analyst_tables(driver, connection))

    columns =
      ~w(air data_source analyst name db_name statement fingerprint)
      |> Stream.map(&quote_identifier(driver, &1))
      |> Enum.join(", ")

    connection
    |> driver.select!("SELECT #{columns} FROM #{quoted_analyst_table_name(driver)}")
    |> Enum.map(fn [air, data_source, analyst, name, db_name, statement, fingerprint] ->
      %{
        air_name: air,
        data_source_name: data_source,
        analyst: to_integer(analyst),
        name: name,
        db_name: db_name,
        statement: Base.decode64!(statement),
        fingerprint: Base.decode64!(fingerprint),
        status: if(MapSet.member?(existing_db_names, db_name), do: :created, else: :creating)
      }
    end)
  end

  defp to_integer(integer) when is_integer(integer), do: integer
  defp to_integer(string) when is_binary(string), do: String.to_integer(string)

  defp drop_table(driver, connection, table) do
    # dropping previous table will make sure that if db_name algorithm is changed, we don't keep the old tables
    drop_previous_table(driver, connection, table)

    # we're also dropping the table with the pending name, to clear a possibly non-registered table (created with the
    # previous version of the cloak)
    driver.execute(connection, "DROP TABLE #{quote_identifier(driver, table.db_name)}")

    delete_meta(driver, connection, table)
  end

  defp drop_previous_table(driver, connection, table) do
    driver.select!(
      connection,
      """
      SELECT #{quote_identifier(driver, "db_name")} FROM #{quoted_analyst_table_name(driver)}
      WHERE #{where_filter(driver, table)}
      """
    )
    |> Enum.to_list()
    |> case do
      [[name]] -> driver.execute(connection, "DROP TABLE #{quote_identifier(driver, name)}")
      [] -> nil
    end
  end

  defp delete_meta(driver, connection, table) do
    driver.execute(
      connection,
      "DELETE FROM #{quoted_analyst_table_name(driver)} WHERE #{where_filter(driver, table)}"
    )
  end

  defp create_table(driver, connection, table, sql) do
    Logger.info("creating database table `#{table.db_name}`")
    with {:ok, _} <- insert_meta(driver, connection, table), do: driver.execute(connection, sql)
  end

  defp insert_meta(driver, connection, table) do
    record = db_record(table)
    columns = record |> Map.keys() |> Stream.map(&quote_identifier(driver, &1)) |> Enum.join(", ")
    values = record |> Map.values() |> Stream.map(&literal(driver, &1)) |> Enum.join(", ")
    driver.execute(connection, "INSERT INTO #{quoted_analyst_table_name(driver)} (#{columns}) VALUES (#{values})")
  end

  defp where_filter(driver, table) do
    table
    |> db_record()
    |> Map.take(~w(air data_source analyst name))
    |> Stream.map(fn {column_name, value} -> "#{quote_identifier(driver, column_name)} = #{literal(driver, value)}" end)
    |> Enum.join(" AND ")
  end

  defp literal(_driver, string) when is_binary(string), do: "'#{SqlBuilder.escape_string(string)}'"
  defp literal(_driver, integer) when is_integer(integer), do: to_string(integer)
  defp literal(driver, {:long_text, string}), do: driver.sql_dialect_module().long_string(string)

  defp db_record(table) do
    %{
      "air" => table.air_name,
      "data_source" => table.data_source_name,
      "analyst" => table.analyst,
      "name" => table.name,
      "db_name" => table.db_name,
      # Note: we're encoding the statement with base64 to avoid possible encoding issues on the underlying database.
      # Base64 should ensure that we can get the exact same statement that was originally submitted to create the table.
      "statement" => {:long_text, Base.encode64(table.statement)},
      # fingerprint is base64 encoded because it's a binary
      "fingerprint" => Base.encode64(table.fingerprint)
    }
  end

  defp quoted_analyst_table_name(driver), do: quote_identifier(driver, @analyst_meta_table_name)

  defp quote_identifier(driver, identifier),
    do: SqlBuilder.quote_table_name(identifier, driver.sql_dialect_module().quote_char())
end
