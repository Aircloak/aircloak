defmodule Cloak.DataSource.MySQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for MySQL / MariaDB.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{SqlBuilder, Table, Driver}
  alias Cloak.DataSource
  alias Cloak.Query.DataDecoder


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Driver

  @impl Driver
  def sql_dialect_module(_parameters), do: Cloak.DataSource.SqlBuilder.MySQL

  @impl Driver
  def connect!(parameters) do
    self = self()
    parameters =
      Enum.to_list(parameters) ++ [types: true, sync_connect: true,
        pool: DBConnection.Connection, timeout: Driver.timeout(), after_connect: fn (_) -> send self, :connected end]
    {:ok, connection} = Mariaex.start_link(parameters)
    receive do
      :connected ->
        {:ok, %Mariaex.Result{}} = Mariaex.query(connection, "SET sql_mode = 'ANSI,NO_BACKSLASH_ESCAPES'", [])
        {:ok, %Mariaex.Result{}} = Mariaex.query(connection, "SET div_precision_increment = 30", [])
        connection
    after :timer.seconds(5)
      ->
        GenServer.stop(connection)
        DataSource.raise_error("Unknown failure during database connection process")
    end
  end
  @impl Driver
  def disconnect(connection) do
    GenServer.stop(connection)
  end

  @impl Driver
  def load_tables(connection, table) do
    query = "SHOW COLUMNS FROM #{table.db_name}"
    column_info_mapper = fn [name, type | _others] -> Table.column(name, parse_type(type)) end
    {:ok, columns} = run_query(connection, query, column_info_mapper, &Enum.to_list/1)
    [%{table | columns: columns}]
  end

  @impl Driver
  def select(connection, sql_query, result_processor) do
    statement = SqlBuilder.build(sql_query)
    field_mappers = for column <- sql_query.db_columns, do:
      column |> DataDecoder.encoded_type() |> type_to_field_mapper()
    run_query(connection, statement, &map_fields(&1, field_mappers), result_processor)
  end

  @impl Driver
  def supports_query?(query), do: SqlBuilder.Support.supports_query?(query)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_query(pool, statement, decode_mapper, result_processor) do
    try do
      Mariaex.transaction(pool, fn(connection) ->
        Mariaex.stream(connection, statement, [], [decode_mapper: decode_mapper, max_rows: Driver.batch_size])
        |> Stream.flat_map(fn (%Mariaex.Result{rows: rows}) -> rows end)
        |> result_processor.()
      end, [timeout: Driver.timeout()])
    rescue
      error in Mariaex.Error -> DataSource.raise_error("Driver exception: `#{Exception.message(error)}`")
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
  defp parse_type("datetime"), do: :datetime
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type(type), do: {:unsupported, type}


  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp map_fields([], []), do: []
  defp map_fields([field | rest_fields], [mapper | rest_mappers]), do:
    [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:real), do: &real_field_mapper/1
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

  defp generic_field_mapper({{year, month, day}, {hour, min, sec, msec}}), do:
    NaiveDateTime.new(year, month, day, hour, min, sec, {msec * 1000, 6}) |> error_to_nil()
  defp generic_field_mapper({year, month, day}), do: Date.new(year, month, day) |> error_to_nil()
  defp generic_field_mapper({hour, min, sec, msec}), do: Time.new(hour, min, sec, {msec * 1000, 6}) |> error_to_nil()
  defp generic_field_mapper(<<0>>), do: false
  defp generic_field_mapper(<<1>>), do: true
  defp generic_field_mapper(value), do: value

  defp interval_field_mapper(nil), do: nil
  defp interval_field_mapper(number), do: Timex.Duration.from_seconds(number)

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil


  # -------------------------------------------------------------------
  # Test functions
  # -------------------------------------------------------------------

  if Mix.env == :test do
    defp parameter_mapper(%NaiveDateTime{} = dt), do:
      {{dt.year, dt.month, dt.day}, {dt.hour, dt.minute, dt.second, 0}}
    defp parameter_mapper(%Date{} = d), do: {d.year, d.month, d.day}
    defp parameter_mapper(%Time{} = t), do: {t.hour, t.minute, t.second, 0}
    defp parameter_mapper(value), do: value

    @doc false
    def execute(connection, statement, parameters \\ [])
    def execute(connection, "DROP SCHEMA " <> _rest = statement, []), do:
      Mariaex.query(connection, String.replace(statement, "CASCADE", ""))
    def execute(connection, "CREATE TABLE " <> _rest = statement, []), do:
      Mariaex.query(connection, String.replace(statement, " BOOLEAN", " BIT"))
    def execute(connection, statement, parameters) do
      parameters = Enum.map(parameters, &parameter_mapper/1)
      statement = statement |> to_string() |> String.replace(~r/\$\d+/, "?")
      Mariaex.query(connection, statement, parameters, [timeout: :timer.minutes(2)])
    end
  end
end
