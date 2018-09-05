defmodule Cloak.DataSource.MySQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for MySQL / MariaDB.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.Table
  alias Cloak.DataSource

  use Cloak.DataSource.Driver.SQL

  require Logger

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect!(parameters) do
    self = self()

    parameters =
      Enum.to_list(parameters) ++
        [
          types: true,
          sync_connect: true,
          pool: DBConnection.Connection,
          timeout: Driver.timeout(),
          after_connect: fn _ -> send(self, :connected) end
        ]

    {:ok, connection} = Mariaex.start_link(parameters)

    receive do
      :connected ->
        {:ok, %Mariaex.Result{}} = Mariaex.query(connection, "SET sql_mode = 'ANSI,NO_BACKSLASH_ESCAPES'", [])

        {:ok, %Mariaex.Result{}} = Mariaex.query(connection, "SET div_precision_increment = 30", [])

        connection
    after
      Driver.connect_timeout() ->
        GenServer.stop(connection, :normal, :timer.seconds(5))

        DataSource.raise_error(
          "Failed to establish a connection to the database. " <>
            "Please check that the database server is running, is reachable from the " <>
            "Insights Cloak host, and the database credentials are correct"
        )
    end
  end

  @impl Driver
  def load_tables(connection, table) do
    query = "SHOW COLUMNS FROM #{SqlBuilder.quote_table_name(table.db_name)}"
    column_info_mapper = fn [name, type | _others] -> Table.column(name, parse_type(type)) end

    case run_query(connection, query, column_info_mapper, &Enum.concat/1) do
      {:ok, columns} -> [%{table | columns: columns}]
      {:error, reason} -> DataSource.raise_error("`#{reason}`")
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
  def supports_connection_sharing?(), do: true

  @impl Driver
  def cast_to_text?(), do: false

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_query(pool, statement, decode_mapper, result_processor) do
    Logger.debug(fn -> "Executing SQL query: #{statement}" end)

    Mariaex.transaction(
      pool,
      fn connection ->
        Mariaex.stream(
          connection,
          statement,
          [],
          decode_mapper: decode_mapper,
          max_rows: Driver.batch_size()
        )
        |> Stream.map(fn %Mariaex.Result{rows: rows} -> rows end)
        |> result_processor.()
      end,
      timeout: Driver.timeout()
    )
  rescue
    error in Mariaex.Error ->
      DataSource.raise_error("Driver exception: `#{Exception.message(error)}`")
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
  defp parse_type("datetime"), do: :datetime
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

  defp real_field_mapper(nil), do: nil
  defp real_field_mapper(%Decimal{} = value), do: Decimal.to_float(value)
  defp real_field_mapper(value) when is_float(value), do: value
  defp real_field_mapper(value) when is_integer(value), do: value * 1.0

  defp boolean_field_mapper(nil), do: nil
  defp boolean_field_mapper(value) when value in [<<0>>, 0, false], do: false
  defp boolean_field_mapper(value) when value in [<<1>>, 1, true], do: true

  defp generic_field_mapper({{year, month, day}, {hour, min, sec, msec}}),
    do: NaiveDateTime.new(year, month, day, hour, min, sec, {msec * 1000, 6}) |> error_to_nil()

  defp generic_field_mapper({year, month, day}), do: Date.new(year, month, day) |> error_to_nil()

  defp generic_field_mapper({hour, min, sec, msec}), do: Time.new(hour, min, sec, {msec * 1000, 6}) |> error_to_nil()

  defp generic_field_mapper(value), do: value

  defp interval_field_mapper(nil), do: nil
  defp interval_field_mapper(number), do: Timex.Duration.from_seconds(number)

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil

  # -------------------------------------------------------------------
  # Test functions
  # -------------------------------------------------------------------

  if Mix.env() == :test do
    defp parameter_mapper(%NaiveDateTime{} = dt), do: {{dt.year, dt.month, dt.day}, {dt.hour, dt.minute, dt.second, 0}}

    defp parameter_mapper(%Date{} = d), do: {d.year, d.month, d.day}
    defp parameter_mapper(%Time{} = t), do: {t.hour, t.minute, t.second, 0}
    defp parameter_mapper(value), do: value

    @doc false
    def execute(connection, statement, parameters \\ [])

    def execute(connection, "DROP SCHEMA " <> _rest = statement, []),
      do: Mariaex.query(connection, String.replace(statement, "CASCADE", ""))

    def execute(connection, "CREATE TABLE " <> _rest = statement, []) do
      statement =
        statement
        |> String.replace(" BOOLEAN", " BIT")
        |> String.replace(" TIMESTAMP", " DATETIME")

      Mariaex.query(connection, statement)
    end

    def execute(connection, statement, parameters) do
      parameters = Enum.map(parameters, &parameter_mapper/1)
      statement = statement |> to_string() |> String.replace(~r/\$\d+/, "?")
      Mariaex.query(connection, statement, parameters, timeout: :timer.minutes(2))
    end
  end
end
