defmodule Cloak.DataSource.SQLServerTds do
  @moduledoc "Implements the DataSource.Driver behaviour for MS SQL Server. For more information, see `DataSource`."

  alias Cloak.DataSource.Table
  alias Cloak.DataSource
  alias Cloak.Query.DataDecoder

  use Cloak.DataSource.Driver.SQL


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_parameters), do: SqlBuilder.SQLServer

  @impl Driver
  def connect!(parameters) do
    self = self()
    parameters = Enum.to_list(parameters) ++ [sync_connect: true,
      pool: DBConnection.Connection, after_connect: fn (_) -> send self, :connected end]
    {:ok, connection} = Tds.start_link(parameters)
    receive do
      :connected ->
        Tds.query!(connection, "SET ANSI_DEFAULTS ON", [])
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
      [full_table_name] -> {"dbo", full_table_name}
      [schema_name, table_name] -> {schema_name, table_name}
    end
    query = "SELECT column_name, data_type FROM information_schema.columns " <>
      "WHERE table_name = '#{table_name}' AND table_schema = '#{schema_name}' ORDER BY ordinal_position DESC"
    row_mapper = fn ([name, type_name]) -> Table.column(name, parse_type(type_name)) end
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
    Tds.transaction(pool, fn(connection) ->
      with {:ok, query} <- Tds.prepare(connection, statement, []) do
        try do
          with {:ok, %Tds.Result{rows: rows}} <- Tds.execute(connection, query, [], [decode_mapper: decode_mapper]) do
            {:ok, result_processor.([rows])}
          end
        after
          Tds.close(connection, query)
        end
      else
        {:error, error} -> DataSource.raise_error("Driver exception: `#{Exception.message(error)}`")
      end
    end, [timeout: Driver.timeout()])
  end

  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("nchar"), do: :text
  defp parse_type("nvarchar"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("ntext"), do: :text
  defp parse_type("bit"), do: :boolean
  defp parse_type("int"), do: :integer
  defp parse_type("bigint"), do: :integer
  defp parse_type("smallint"), do: :integer
  defp parse_type("tinyint"), do: :integer
  defp parse_type("real"), do: :real
  defp parse_type("float"), do: :real
  defp parse_type("money"), do: :real
  defp parse_type("smallmoney"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("uniqueidentifier"), do: :text
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type("datetime"), do: :datetime
  defp parse_type("datetime2"), do: :datetime
  defp parse_type("smalldatetime"), do: :datetime
  defp parse_type("datetimeoffset"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}


  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp map_fields([], []), do: []
  defp map_fields([field | rest_fields], [mapper | rest_mappers]), do:
    [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:real), do: &real_field_mapper/1
  defp type_to_field_mapper(:datetime), do: &datetime_field_mapper/1
  defp type_to_field_mapper(:date), do: &date_field_mapper/1
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

  defp datetime_field_mapper(nil), do: nil
  defp datetime_field_mapper({date, time, _offset}), do: datetime_field_mapper({date, time})
  defp datetime_field_mapper({{year, month, day}, {hour, min, sec, fsec}}), do:
    NaiveDateTime.new(year, month, day, hour, min, sec, {div(fsec, 10), 6}) |> error_to_nil()

  defp date_field_mapper(nil), do: nil
  defp date_field_mapper({year, month, day}), do:
    Date.new(year, month, day) |> error_to_nil()

  defp time_field_mapper(nil), do: nil
  defp time_field_mapper({hour, min, sec, fsec}), do:
    Time.new(hour, min, sec, {div(fsec, 10), 6}) |> error_to_nil()

  defp interval_field_mapper(nil), do: nil
  defp interval_field_mapper(number), do: Timex.Duration.from_seconds(number)

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil

  defp generic_field_mapper(value), do: value
end
