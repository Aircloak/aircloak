defmodule Cloak.DataSource.ODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for ODBC compatible data-stores.
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
  def sql_dialect_module(%{dialect: dialect}), do: dialect
  def sql_dialect_module(%{'DSN': dsn}), do:
    # Only needed for dev/test, where we access PostgreSQL through an ODBC data source.
    dsn
    |> String.downcase()
    |> dialect_module()

  @impl Driver
  def connect!(parameters) do
    options = [auto_commit: :on, binary_strings: :on, tuple_row: :off, timeout: Driver.connect_timeout()]
    with {:ok, connection} <- parameters |> to_connection_string() |> :odbc.connect(options) do
      parameters |> sql_dialect_module() |> init_connection(connection)
      connection
    else
      {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  @impl Driver
  def disconnect(connection), do: :odbc.disconnect(connection)

  @impl Driver
  def load_tables(connection, table) do
    case :odbc.describe_table(connection, to_charlist(table.db_name), _timeout = :timer.seconds(30)) do
      {:ok, columns} ->
        columns = for {name, type} <- columns, do: Table.column(to_string(name), parse_type(type))
        [%{table | columns: columns}]
      {:error, reason} ->
        DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  @impl Driver
  def select(connection, sql_query, result_processor) do
    statement = sql_query |> SqlBuilder.build() |> to_charlist()
    field_mappers = for column <- sql_query.db_columns, do:
      column |> DataDecoder.encoded_type() |> type_to_field_mapper(sql_query.data_source)
    case :odbc.select_count(connection, statement, Driver.timeout()) do
      {:ok, _count} ->
        data_stream = Stream.resource(
          fn () -> connection end,
          fn (conn) ->
            case :odbc.select(conn, :next, Driver.batch_size(), Driver.timeout()) do
              {:selected, _columns, []} -> {:halt, conn}
              {:selected, _columns, rows} -> {[Enum.map(rows, &map_fields(&1, field_mappers))], conn}
              {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
            end
          end,
          fn (_conn) -> :ok end
        )
        {:ok, result_processor.(data_stream)}
      {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp dialect_module("postgresql"), do: SqlBuilder.PostgreSQL

  defp to_connection_string(parameters) do
    parameters
    |> Enum.map(fn({key, value}) ->
      if value |> to_string() |> String.contains?([";", "{"]), do:
        DataSource.raise_error("The characters ';' and '{' are not allowed inside ODBC driver parameters!")
      "#{Atom.to_string(key)}=#{value}"
    end)
    |> Enum.join(";")
    |> to_charlist()
  end

  defp init_connection(SqlBuilder.PostgreSQL, connection), do:
    {:updated, _} = :odbc.sql_query(connection, 'SET standard_conforming_strings = ON')
  defp init_connection(_, _connection), do:
    :ok

  defp parse_type(:sql_integer), do: :integer
  defp parse_type(:sql_smallint), do: :integer
  defp parse_type(:sql_tinyint), do: :integer
  defp parse_type(:SQL_BIGINT), do: :integer
  defp parse_type(:sql_bit), do: :boolean
  defp parse_type(:sql_real), do: :real
  defp parse_type(:sql_float), do: :real
  defp parse_type(:sql_double), do: :real
  defp parse_type(:SQL_LONGVARCHAR), do: :text
  defp parse_type(:SQL_VARBINARY), do: :text
  defp parse_type(:SQL_LONGVARBINARY), do: :text
  defp parse_type({:sql_varchar, _length}), do: :text
  defp parse_type({:sql_wvarchar, _length}), do: :text
  defp parse_type({:sql_wchar, _length}), do: :text
  defp parse_type({:sql_char, _length}), do: :text
  defp parse_type({:sql_wlongvarchar, _length}), do: :text
  defp parse_type(:sql_timestamp), do: :datetime
  defp parse_type(:SQL_TYPE_DATE), do: :date
  defp parse_type(:SQL_TYPE_TIME), do: :time
  defp parse_type({:sql_numeric, _, _}), do: :real
  defp parse_type({:sql_decimal, _, _}), do: :real
  defp parse_type({:sql_float, _}), do: :real
  defp parse_type(type), do: {:unsupported, type}

  defp map_fields([], []), do: []
  defp map_fields([field | rest_fields], [mapper | rest_mappers]), do:
    [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:datetime, _data_source), do: &datetime_field_mapper/1
  defp type_to_field_mapper(:time, _data_source), do: &time_field_mapper/1
  defp type_to_field_mapper(:date, _data_source), do: &date_field_mapper/1
  defp type_to_field_mapper(:real, _data_source), do: &real_field_mapper/1
  defp type_to_field_mapper(:integer, _data_source), do: &integer_field_mapper/1
  defp type_to_field_mapper(:text, %{parameters: %{encoding: encoding}}) when encoding != nil, do:
    text_to_unicode_mapper(encoding)
  # We hardcode the default encoding for SQL Server and SAP HANA to be utf16 little endian.
  # This is for historic reasons more than anything, since that's what our servers are using internally.
  defp type_to_field_mapper(:text, %{driver: Cloak.DataSource.SQLServer}), do: text_to_unicode_mapper({:utf16, :little})
  defp type_to_field_mapper(:text, %{driver: Cloak.DataSource.SAPHana}), do: text_to_unicode_mapper({:utf16, :little})
  defp type_to_field_mapper(:interval, data_source), do: &interval_field_mapper(&1, data_source)
  defp type_to_field_mapper(_, _data_source), do: &generic_field_mapper/1

  defp generic_field_mapper(:null), do: nil
  defp generic_field_mapper(value), do: value

  defp datetime_field_mapper(:null), do: nil
  defp datetime_field_mapper({{year, month, day}, {hour, min, sec}}) when is_integer(sec), do:
    NaiveDateTime.new(year, month, day, hour, min, sec, {0, 6}) |> error_to_nil()
  defp datetime_field_mapper({{year, month, day}, {hour, min, fsec}}) when is_float(fsec) do
    sec = trunc(fsec)
    usec = {trunc((fsec - sec) * 1_000_000), 6}
    NaiveDateTime.new(year, month, day, hour, min, sec, usec) |> error_to_nil()
  end

  defp date_field_mapper(:null), do: nil
  defp date_field_mapper(string) when is_binary(string), do:
    Cloak.Time.parse_date(string) |> error_to_nil()

  defp time_field_mapper(:null), do: nil
  defp time_field_mapper(string) when is_binary(string), do:
    Cloak.Time.parse_time(string) |> error_to_nil()

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil

  defp real_field_mapper(:null), do: nil
  defp real_field_mapper(value) when is_binary(value) do
    value
    |> Float.parse()
    |> case do
      {value, ""} -> value
      {_, "," <> _} -> String.to_float(value)
    end
  end
  defp real_field_mapper(value) when is_float(value), do: value
  defp real_field_mapper(value) when is_integer(value), do: value * 1.0

  defp integer_field_mapper(:null), do: nil
  defp integer_field_mapper(value) when is_binary(value) do
    {value, ""} = Integer.parse(value)
    value
  end
  defp integer_field_mapper(value) when is_integer(value), do: value
  defp integer_field_mapper(value) when is_float(value), do: round(value)

  defp interval_field_mapper(:null, _data_source), do: nil
  defp interval_field_mapper(string, %{driver: Cloak.DataSource.SAPHana}) when is_binary(string), do:
    string |> String.to_integer() |> Timex.Duration.from_seconds()
  defp interval_field_mapper(number, _data_source), do: Timex.Duration.from_seconds(number)

  defp text_to_unicode_mapper(encoding), do:
    fn
      (:null) -> nil
      (value) -> :unicode.characters_to_binary(value, encoding)
    end
end
