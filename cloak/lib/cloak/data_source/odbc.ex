defmodule Cloak.DataSource.ODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for ODBC compatible data-stores.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{SqlBuilder, Table}
  alias Cloak.DataSource
  alias Cloak.Query.DataDecoder


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @doc false
  def dialect(%{'DSN': dsn}), do: dsn |> String.downcase() |> String.to_existing_atom()

  @doc false
  def connect!(parameters) do
    options = [auto_commit: :on, binary_strings: :on, tuple_row: :off]
    with {:ok, connection} <- parameters |> to_connection_string() |> :odbc.connect(options) do
      parameters |> dialect() |> set_dialect(connection)
      connection
    else
      {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  @doc false
  def disconnect(connection), do: :odbc.disconnect(connection)

  @doc false
  def load_tables(connection, table) do
    case :odbc.describe_table(connection, to_char_list(table.db_name), _timeout = :timer.seconds(15)) do
      {:ok, columns} ->
        columns = for {name, type} <- columns, do: Table.column(to_string(name), parse_type(type))
        [%{table | columns: columns}]
      {:error, reason} ->
        DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  @doc false
  def select(connection, sql_query, result_processor) do
    statement = sql_query |> SqlBuilder.build() |> to_char_list()
    field_mappers = for column <- sql_query.db_columns, do:
      column |> DataDecoder.encoded_type() |> type_to_field_mapper(sql_query.data_source)
    case :odbc.select_count(connection, statement, _timeout = :timer.hours(4)) do
      {:ok, _count} ->
        data_stream = Stream.resource(fn () -> connection end, fn (conn) ->
          case :odbc.select(conn, :next, _rows_per_batch = 25_000, _timeout = :timer.minutes(30)) do
            {:selected, _columns, []} -> {:halt, conn}
            {:selected, _columns, rows} -> {Enum.map(rows, &map_fields(&1, field_mappers)), conn}
            {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
          end
        end, fn (_conn) -> :ok end)
        {:ok, result_processor.(data_stream)}
      {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  @doc false
  def supports_query?(query), do: SqlBuilder.Support.supported_query?(query)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp to_connection_string(parameters) do
    parameters
    |> Enum.map(fn({key, value}) ->
      if value |> to_string() |> String.contains?([";", "{"]), do:
        DataSource.raise_error("The characters ';' and '{' are not allowed inside ODBC driver parameters!")
      "#{Atom.to_string(key)}=#{value}"
    end)
    |> Enum.join(";")
    |> to_char_list()
  end

  defp set_dialect(:mysql, connection), do:
    {:updated, _} = :odbc.sql_query(connection, 'SET sql_mode = "ANSI,NO_BACKSLASH_ESCAPES"')
  defp set_dialect(:postgresql, connection), do:
    {:updated, _} = :odbc.sql_query(connection, 'SET standard_conforming_strings = ON')
  defp set_dialect(:sqlserver, connection), do:
    {:updated, _} = :odbc.sql_query(connection, 'SET ANSI_DEFAULTS ON')
  defp set_dialect(:drill, _connection), do: :ok

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
  # We hardcode the default encoding for SQL Server to be utf16 little endian.
  # This is for historic reasons more than anything, since that's what our SQL Servers
  # are using internally.
  defp type_to_field_mapper(:text, %{driver_dialect: :sqlserver}), do: text_to_unicode_mapper({:utf16, :little})
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
    {value, ""} = Float.parse(value)
    value
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

  defp text_to_unicode_mapper(encoding), do:
    fn
      (:null) -> nil
      (value) -> :unicode.characters_to_binary(value, encoding)
    end
end
