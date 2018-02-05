defmodule Cloak.DataSource.PostgreSQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for PostgreSQL.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.Table
  alias Cloak.DataSource
  alias Cloak.Query.DataDecoder

  use Cloak.DataSource.Driver.SQL


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect!(parameters) do
    self = self()
    parameters = Enum.to_list(parameters) ++ [types: Postgrex.DefaultTypes, sync_connect: true,
      pool: DBConnection.Connection, after_connect: fn (_) -> send self, :connected end]
    {:ok, connection} = Postgrex.start_link(parameters)
    receive do
      :connected ->
        {:ok, %Postgrex.Result{}} = Postgrex.query(connection, "SET standard_conforming_strings = ON", [])
        connection
    after :timer.seconds(5)
      ->
        GenServer.stop(connection, :normal, :timer.seconds(5))
        DataSource.raise_error("Unknown failure during database connection process")
    end
  end

  @impl Driver
  def load_tables(connection, table) do
    {schema_name, table_name} = case String.split(table.db_name, ".") do
      [full_table_name] -> {"public", full_table_name}
      [schema_name, table_name] -> {schema_name, table_name}
    end
    query = "SELECT column_name, udt_name FROM information_schema.columns " <>
      "WHERE table_name = '#{table_name}' AND table_schema = '#{schema_name}'"
    row_mapper = fn [name, type_name] -> Table.column(name, parse_type(type_name)) end
    case run_query(connection, query, row_mapper, &Enum.concat/1) do
      {:ok, []} -> DataSource.raise_error("Table `#{table.db_name}` does not exist")
      {:ok, columns} -> [%{table | columns: columns}]
      {:error, reason} -> DataSource.raise_error("`#{reason}`")
    end
  end

  @impl Driver
  def select(connection, sql_query, result_processor) do
    statement = SqlBuilder.build(sql_query)
    field_mappers = for column <- sql_query.db_columns, do:
      column |> DataDecoder.encoded_type() |> type_to_field_mapper()
    run_query(connection, statement, &map_fields(&1, field_mappers), result_processor)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_query(pool, statement, decode_mapper, result_processor) do
    Postgrex.transaction(pool, fn(connection) ->
      with {:ok, query} <- Postgrex.prepare(connection, "data select", statement, []) do
        try do
          Postgrex.stream(connection, query, [], [decode_mapper: decode_mapper, max_rows: Driver.batch_size()])
          |> Stream.map(fn (%Postgrex.Result{rows: rows}) -> rows end)
          |> result_processor.()
        after
          Postgrex.close(connection, query)
        end
      else
        {:error, error} -> DataSource.raise_error("Driver exception: `#{Exception.message(error)}`")
      end
    end, [timeout: Driver.timeout()])
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


  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp map_fields([], []), do: []
  defp map_fields([field | rest_fields], [mapper | rest_mappers]), do:
    [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:real), do: &real_field_mapper/1
  defp type_to_field_mapper(:time), do: &time_field_mapper/1
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

  defp time_field_mapper(%Postgrex.Interval{days: 0, months: 0, secs: secs}), do: Cloak.Time.from_integer(secs, :time)
  defp time_field_mapper(value), do: value

  defp interval_field_mapper(%Postgrex.Interval{months: m, days: d, secs: s}), do:
    Timex.Duration.parse!("P#{m}M#{d}DT#{s}S")
  defp interval_field_mapper(value), do: value

  defp generic_field_mapper(value), do: value


  # -------------------------------------------------------------------
  # Test functions
  # -------------------------------------------------------------------

  @doc false
  def execute(connection, statement, parameters), do:
    Postgrex.query(connection, statement, parameters, [timeout: :timer.minutes(2)])
end
