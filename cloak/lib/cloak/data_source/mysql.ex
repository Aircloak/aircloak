defmodule Cloak.DataSource.MySQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for MySQL / MariaDB.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{SqlBuilder, Table}
  alias Cloak.Query.ExecutionError

  use Cloak.DataSource.Driver.SQL

  require Logger

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters) do
    with {:ok, connection} <- do_connect(parameters) do
      MyXQL.query!(connection, "SET sql_mode = 'REAL_AS_FLOAT,PIPES_AS_CONCAT,IGNORE_SPACE,NO_BACKSLASH_ESCAPES'")
      MyXQL.query!(connection, "SET div_precision_increment = 30")
      {:ok, connection}
    end
  end

  @impl Driver
  def load_tables(connection, table) do
    query = "SHOW COLUMNS FROM #{SqlBuilder.quote_table_name(table.db_name, SqlBuilder.MySQL.quote_char())}"
    column_info_mapper = fn [name, type | _others] -> Table.column(name, parse_type(type)) end

    case run_query(connection, query, column_info_mapper, &Enum.concat/1) do
      {:ok, columns} -> [load_comments(connection, %{table | columns: columns})]
      {:error, reason} -> raise ExecutionError, message: "`#{reason}`"
    end
  end

  @impl Driver
  def select(connection, sql_query, result_processor) do
    statement = SqlBuilder.build(sql_query)
    field_mappers = Enum.map(sql_query.db_columns, &type_to_field_mapper(&1.type))
    run_query(connection, statement, &map_fields(&1, field_mappers), result_processor)
  end

  @impl Driver
  def driver_info(_connection), do: nil

  @impl Driver
  def supports_query?(query), do: length(query.grouping_sets) <= 1 or query.type == :anonymized

  # -------------------------------------------------------------------
  # DataSource.Driver.SQL callbacks
  # -------------------------------------------------------------------

  @impl Driver.SQL
  def execute(connection, sql),
    do: with({:error, error} <- MyXQL.query(connection, sql), do: {:error, Exception.message(error)})

  @impl Driver.SQL
  def select(connection, sql), do: with({:ok, result} <- execute(connection, sql), do: {:ok, result.rows})

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp absolute_filepath(filepath), do: Application.app_dir(:cloak, ["priv", "config", filepath]) |> Path.expand()

  defp do_connect(parameters) do
    self = self()

    parameters =
      parameters
      |> Enum.to_list()
      |> Keyword.merge(
        types: true,
        after_connect: fn _ -> send(self, :connected) end,
        backoff_type: :stop,
        max_restarts: 0,
        timeout: Driver.timeout()
      )
      |> update_in([Lens.key?(:ssl_opts) |> Lens.keys?([:certfile, :keyfile, :cacertfile])], &absolute_filepath/1)
      |> update_in([Lens.key?(:ssl_opts)], &Enum.to_list/1)

    case MyXQL.start_link(parameters) do
      {:ok, connection} ->
        receive do
          :connected -> {:ok, connection}
        after
          Driver.connect_timeout() -> {:error, "Timeout connecting to server."}
        end

      {:error, {%MyXQL.Error{} = error, _stacktrace}} ->
        {:error, error.message}
    end
  end

  defp run_query(pool, statement, decode_mapper, result_processor) do
    Logger.debug(fn -> "Executing SQL query: #{statement}" end)

    MyXQL.transaction(
      pool,
      fn connection ->
        MyXQL.stream(
          connection,
          statement,
          [],
          max_rows: Driver.batch_size()
        )
        |> Stream.map(fn %MyXQL.Result{rows: rows} -> Enum.map(rows, decode_mapper) end)
        |> result_processor.()
      end,
      timeout: Driver.timeout()
    )
  rescue
    error in MyXQL.Error -> {:error, Exception.message(error)}
  end

  defp load_comments(connection, table) do
    filter = table_filter(table)

    table_comments =
      connection
      |> select("""
        SELECT TABLE_COMMENT
        FROM INFORMATION_SCHEMA.TABLES
        WHERE #{filter}
      """)
      |> case do
        {:ok, [[comment]]} -> comment
        _ -> nil
      end

    column_comments =
      connection
      |> select("""
        SELECT COLUMN_NAME, COLUMN_COMMENT
        FROM INFORMATION_SCHEMA.COLUMNS
        WHERE #{filter} AND COLUMN_COMMENT IS NOT NULL
      """)
      |> case do
        {:ok, results} ->
          results
          |> Enum.map(&List.to_tuple/1)
          |> Enum.into(%{})

        {:error, _} ->
          %{}
      end

    comments =
      %{table: table_comments, columns: column_comments}
      |> Aircloak.deep_merge(Map.get(table, :comments, %{}))

    Map.put(table, :comments, comments)
  end

  defp table_filter(table) do
    case SqlBuilder.table_name_parts(table.db_name) do
      [table_name] -> "TABLE_NAME = '#{table_name}'"
      [table_schema, table_name] -> "TABLE_SCHEMA = '#{table_schema}' AND TABLE_NAME = '#{table_name}'"
    end
  end

  defp parse_type("varchar" <> _size), do: :text
  defp parse_type("char" <> _size), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("bit(1)"), do: :boolean
  defp parse_type("int" <> _size), do: :integer
  defp parse_type("tinyint" <> _size), do: :integer
  defp parse_type("smallint" <> _size), do: :integer
  defp parse_type("mediumint" <> _size), do: :integer
  defp parse_type("bigint" <> _size), do: :integer
  defp parse_type("float"), do: :real
  defp parse_type("double"), do: :real
  defp parse_type("decimal" <> _size), do: :real
  defp parse_type("numeric" <> _size), do: :real
  defp parse_type("timestamp"), do: :datetime
  defp parse_type("datetime" <> _size), do: :datetime
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type(type), do: {:unsupported, type}

  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp map_fields([], []), do: []

  defp map_fields([field | rest_fields], [mapper | rest_mappers]),
    do: [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:real), do: &real_field_mapper/1
  defp type_to_field_mapper(:boolean), do: &boolean_field_mapper/1
  defp type_to_field_mapper(:interval), do: &interval_field_mapper/1
  defp type_to_field_mapper(_), do: &generic_field_mapper/1

  defp integer_field_mapper(nil), do: nil
  defp integer_field_mapper(%Decimal{} = value), do: Decimal.to_integer(value)
  defp integer_field_mapper(value) when is_integer(value), do: value
  defp integer_field_mapper(value) when is_float(value), do: round(value)

  defp real_field_mapper(%Decimal{} = value), do: Decimal.to_float(value)
  defp real_field_mapper(value) when is_float(value), do: value
  defp real_field_mapper(value) when is_integer(value), do: value * 1.0
  defp real_field_mapper(_), do: nil

  defp boolean_field_mapper(nil), do: nil
  defp boolean_field_mapper(value) when value in [<<0::size(1)>>, <<0>>, 0, false], do: false
  defp boolean_field_mapper(value) when value in [<<1::size(1)>>, <<1>>, 1, true], do: true

  defp generic_field_mapper(value = %DateTime{}), do: value |> DateTime.to_naive() |> Cloak.Time.max_precision()
  defp generic_field_mapper(value = %Time{}), do: Cloak.Time.max_precision(value)
  defp generic_field_mapper(value), do: value

  defp interval_field_mapper(nil), do: nil
  defp interval_field_mapper(number), do: Timex.Duration.from_seconds(number)

  # -------------------------------------------------------------------
  # Test functions
  # -------------------------------------------------------------------

  if Mix.env() == :test do
    defp parameter_mapper(%NaiveDateTime{} = dt), do: {{dt.year, dt.month, dt.day}, {dt.hour, dt.minute, dt.second, 0}}

    defp parameter_mapper(%Date{} = d), do: {d.year, d.month, d.day}
    defp parameter_mapper(%Time{} = t), do: {t.hour, t.minute, t.second, 0}
    defp parameter_mapper(value), do: value

    @doc false
    def execute(connection, statement, parameters)

    def execute(connection, "DROP SCHEMA " <> _rest = statement, []),
      do: MyXQL.query(connection, String.replace(statement, "CASCADE", ""))

    def execute(connection, "CREATE TABLE " <> _rest = statement, []) do
      statement =
        statement
        |> String.replace(" BOOLEAN", " BIT")
        |> String.replace(" TIMESTAMP", " DATETIME")

      MyXQL.query(connection, statement)
    end

    def execute(connection, statement, parameters) do
      parameters = Enum.map(parameters, &parameter_mapper/1)
      statement = statement |> to_string() |> String.replace(~r/\$\d+/, "?")
      MyXQL.query(connection, statement, parameters, timeout: :timer.minutes(2))
    end
  end
end
