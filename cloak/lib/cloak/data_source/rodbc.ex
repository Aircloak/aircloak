defmodule Cloak.DataSource.RODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for ODBC compatible data-stores, using the Rust port driver.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{RODBC.Driver, SqlBuilder}
  alias Cloak.DataSource


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  def connect!(parameters) do
    port = Driver.open()
    with :ok <- Driver.connect(port, to_connection_string(parameters)) do
      port
    else
      {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  def disconnect(port) do
    true = Driver.close(port)
    :ok
  end

  def select(port, sql_query, result_processor) do
    statement = SqlBuilder.build(sql_query)
    field_mappers = Enum.map(sql_query.db_columns, &type_to_field_mapper(&1.type))
    row_mapper = &map_fields(&1, field_mappers)
    case Driver.execute(port, statement) do
      :ok -> {:ok, port |> stream_rows(row_mapper) |> result_processor.()}
      {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  def driver_info(_connection), do: nil

  def supports_connection_sharing?(), do: true


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
  end

  defp stream_rows(port, row_mapper) do
    Stream.resource(fn () -> port end,
      fn (port) ->
        case Driver.fetch_batch(port, row_mapper, Cloak.DataSource.Driver.batch_size()) do
          {:ok, []} -> {:halt, port}
          {:ok, rows} -> {[rows], port}
          {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
        end
      end, fn (_port) -> :ok end)
  end

  defp map_fields([], []), do: []
  defp map_fields([field | rest_fields], [mapper | rest_mappers]), do:
    [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:datetime), do: &datetime_field_mapper/1
  defp type_to_field_mapper(:time), do: &time_field_mapper/1
  defp type_to_field_mapper(:date), do: &date_field_mapper/1
  defp type_to_field_mapper(:real), do: &real_field_mapper/1
  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:interval), do: &interval_field_mapper(&1)
  defp type_to_field_mapper(_), do: &generic_field_mapper/1

  defp generic_field_mapper(nil), do: nil
  defp generic_field_mapper(value), do: value

  defp datetime_field_mapper(nil), do: nil
  defp datetime_field_mapper(string) when is_binary(string), do:
    Cloak.Time.parse_datetime(string) |> error_to_nil()

  defp date_field_mapper(nil), do: nil
  defp date_field_mapper(string) when is_binary(string), do:
    Cloak.Time.parse_date(string) |> error_to_nil()

  defp time_field_mapper(nil), do: nil
  defp time_field_mapper(string) when is_binary(string), do:
    Cloak.Time.parse_time(string) |> error_to_nil()

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil

  defp real_field_mapper(nil), do: nil
  defp real_field_mapper("." <> rest), do: real_field_mapper("0." <> rest)
  defp real_field_mapper("-." <> rest), do: real_field_mapper("-0." <> rest)
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

  defp integer_field_mapper(nil), do: nil
  defp integer_field_mapper(value) when is_binary(value) do
    {value, ""} = Integer.parse(value)
    value
  end
  defp integer_field_mapper(value) when is_integer(value), do: value
  defp integer_field_mapper(value) when is_float(value), do: round(value)

  defp interval_field_mapper(nil), do: nil
  defp interval_field_mapper(string) when is_binary(string), do:
    string |> String.to_integer() |> Timex.Duration.from_seconds()
  defp interval_field_mapper(number), do: Timex.Duration.from_seconds(number)
end
