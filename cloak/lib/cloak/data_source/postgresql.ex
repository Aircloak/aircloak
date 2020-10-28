defmodule Cloak.DataSource.PostgreSQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for PostgreSQL.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  use Cloak.DataSource.Driver.SQL.AnalystTables
  require Logger
  alias Cloak.DataSource.Table
  alias Cloak.Query.ExecutionError

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters) do
    with {:ok, connection} <- do_connect(parameters) do
      execute!(connection, "SET standard_conforming_strings = ON")
      register_udfs!(connection)
      {:ok, connection}
    end
  end

  @impl Driver
  def load_tables(connection, table) do
    {schema_name, table_name} = table_name_parts(table)

    query =
      "SELECT column_name, udt_name FROM information_schema.columns " <>
        "WHERE table_name = '#{table_name}' AND table_schema = '#{schema_name}'"

    row_mapper = fn [name, type_name] -> Table.column(name, parse_type(type_name)) end

    case run_query(connection, query, row_mapper, &Enum.concat/1) do
      {:ok, []} -> raise ExecutionError, message: "Table `#{table.db_name}` does not exist"
      {:ok, columns} -> [%{table | columns: columns}]
      {:error, reason} -> raise ExecutionError, message: "`#{reason}`"
    end
  end

  @impl Driver
  def load_comments(connection, table) do
    {schema_name, table_name} = table_name_parts(table)

    table_comment =
      connection
      |> select(~s/SELECT obj_description('"#{schema_name}"."#{table_name}"'::regclass)/)
      |> case do
        {:ok, [[comment]]} -> comment
        _ -> nil
      end

    column_comments =
      connection
      |> select("""
        SELECT
          cols.column_name,
          pg_catalog.col_description(c.oid, cols.ordinal_position::int) AS column_comment
        FROM information_schema.columns cols
        INNER JOIN pg_catalog.pg_class c
        ON
          c.oid     = cols.table_name::regclass::oid AND
          c.relname = cols.table_name
        WHERE
          cols.table_schema = '#{schema_name}' AND
          cols.table_name   = '#{table_name}' AND
          pg_catalog.col_description(c.oid, cols.ordinal_position::int) IS NOT NULL
      """)
      |> case do
        {:ok, results} ->
          results
          |> Enum.map(&List.to_tuple/1)
          |> Enum.into(%{})

        _ ->
          %{}
      end

    {table_comment, column_comments}
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
  def supports_analyst_tables?(), do: true

  # -------------------------------------------------------------------
  # DataSource.Driver.SQL callbacks
  # -------------------------------------------------------------------

  @impl Driver.SQL
  def execute(connection, sql),
    do: with({:error, error} <- Postgrex.query(connection, sql, []), do: {:error, Exception.message(error)})

  @impl Driver.SQL
  def select(connection, sql), do: with({:ok, result} <- execute(connection, sql), do: {:ok, result.rows})

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp do_connect(parameters) do
    parameters =
      Enum.to_list(parameters) ++
        [
          types: Postgrex.DefaultTypes,
          sync_connect: true,
          backoff_type: :stop,
          max_restarts: 0
        ]

    case Postgrex.start_link(parameters) do
      {:ok, connection} -> {:ok, connection}
      {:error, {%DBConnection.ConnectionError{} = error, _stacktrace}} -> {:error, error.message}
    end
  end

  defp run_query(pool, statement, decode_mapper, result_processor) do
    Logger.debug(fn -> "Executing SQL query: #{statement}" end)

    Postgrex.transaction(
      pool,
      fn connection ->
        with {:ok, query} <- Postgrex.prepare(connection, "data select", statement, []) do
          try do
            Postgrex.stream(
              connection,
              query,
              [],
              decode_mapper: decode_mapper,
              max_rows: Driver.batch_size()
            )
            |> Stream.map(fn %Postgrex.Result{rows: rows} -> rows end)
            |> result_processor.()
          after
            safe_close(connection, query)
          end
        else
          {:error, error} -> Postgrex.rollback(connection, Exception.message(error))
        end
      end,
      timeout: Driver.timeout()
    )
  end

  defp table_name_parts(table) do
    case SqlBuilder.table_name_parts(table.db_name) do
      [full_table_name] -> {"public", full_table_name}
      [schema_name, table_name] -> {schema_name, table_name}
    end
  end

  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("bpchar"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("bool"), do: :boolean
  defp parse_type("int2"), do: :integer
  defp parse_type("int4"), do: :integer
  defp parse_type("int8"), do: :integer
  defp parse_type("float4"), do: :real
  defp parse_type("float8"), do: :real
  defp parse_type("money"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("decimal"), do: :real
  defp parse_type("timestamp"), do: :datetime
  defp parse_type("timestamptz"), do: :datetime
  defp parse_type("time"), do: :time
  defp parse_type("timetz"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type(type), do: {:unsupported, type}

  defp safe_close(connection, query) do
    Postgrex.close(connection, query)
  rescue
    exception ->
      {exception, __STACKTRACE__}
      |> Cloak.LoggerTranslator.format_exit()
      |> Logger.error()

      :ignore
  end

  @cast_udfs [
    {"ac_text_to_integer(value TEXT) RETURNS BIGINT", "CAST(value AS BIGINT)"},
    {"ac_text_to_real(value TEXT) RETURNS DOUBLE PRECISION", "CAST(value AS DOUBLE PRECISION)"},
    {"ac_text_to_date(value TEXT) RETURNS DATE", "CAST(value AS DATE)"},
    {"ac_text_to_time(value TEXT) RETURNS TIME", "CAST(value AS TIME)"},
    {"ac_text_to_datetime(value TEXT) RETURNS TIMESTAMP", "CAST(value AS TIMESTAMP)"}
  ]

  defp register_udfs!(connection) do
    for {header, body} <- udfs() do
      function = """
      CREATE OR REPLACE FUNCTION pg_temp.#{header} AS $$
      BEGIN
        RETURN #{body};
      EXCEPTION WHEN OTHERS THEN
        RETURN NULL;
      END;
      $$ LANGUAGE PLPGSQL IMMUTABLE;
      """

      execute!(connection, function)
    end
  end

  defp udfs(), do: @cast_udfs ++ math_udfs() ++ date_udfs()

  defp math_udfs() do
    operators = [{"ac_mul", "*"}, {"ac_add", "+"}, {"ac_sub", "-"}, {"ac_div", "/"}, {"ac_pow", "^"}]
    number_types = ["BIGINT", "DOUBLE PRECISION"]

    for type <- number_types, {name, operator} <- operators do
      {"#{name}(a #{type}, b #{type}) RETURNS #{type}", "CAST(a #{operator} b AS #{type})"}
    end
  end

  defp date_udfs() do
    date_functions = [
      {"ac_add(a DATE, b INTERVAL) RETURNS TIMESTAMP", "a + b"},
      {"ac_add(a TIMESTAMP, b INTERVAL) RETURNS TIMESTAMP", "a + b"},
      {"ac_sub(a DATE, b INTERVAL) RETURNS TIMESTAMP", "a - b"},
      {"ac_sub(a TIMESTAMP, b INTERVAL) RETURNS TIMESTAMP", "a - b"}
    ]

    for {header, expression} <- date_functions do
      # Postgrex library crashes when reading values outside the valid Elixir range.
      body = """
        CASE WHEN EXTRACT(year FROM #{expression}) BETWEEN #{Cloak.Time.year_lower_bound()}
          AND #{Cloak.Time.year_upper_bound()} THEN #{expression} ELSE NULL END
      """

      {header, body}
    end
  end

  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp map_fields([], []), do: []
  defp map_fields([_], []), do: []

  defp map_fields([field | rest_fields], [mapper | rest_mappers]),
    do: [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:real), do: &real_field_mapper/1
  defp type_to_field_mapper(:time), do: &time_field_mapper/1
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

  defp time_field_mapper(%Postgrex.Interval{days: 0, months: 0, secs: secs}), do: Cloak.Time.from_integer(secs, :time)

  defp time_field_mapper(value), do: value

  defp interval_field_mapper(%Postgrex.Interval{months: m, days: d, secs: s}),
    do: Timex.Duration.parse!("P#{abs(m)}M#{abs(d)}DT#{abs(s)}S")

  defp interval_field_mapper(value), do: value

  defp generic_field_mapper(value = %DateTime{}), do: value |> DateTime.to_naive() |> Cloak.Time.max_precision()
  defp generic_field_mapper(value), do: value

  # -------------------------------------------------------------------
  # Test functions
  # -------------------------------------------------------------------

  @doc false
  def execute(connection, statement, parameters),
    do: Postgrex.query(connection, statement, parameters, timeout: :timer.minutes(2))
end
