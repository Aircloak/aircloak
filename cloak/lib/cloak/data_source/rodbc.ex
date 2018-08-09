defmodule Cloak.DataSource.RODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for ODBC compatible data-stores, using the Rust port driver.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{RODBC.Driver, SqlBuilder, Table}
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Normalizes the connection parameters and connects to the data source via odbc."
  @spec connect!(Driver.parameters(), (map -> map), Keyword.t()) :: :odbc.connection_reference()
  def connect!(parameters, conn_params_extractor, driver_params \\ []) do
    normalized_parameters = Cloak.DataSource.ODBC.normalize_parameters(parameters)

    normalized_parameters
    |> conn_params_extractor.()
    |> Map.merge(Map.get(normalized_parameters, :odbc_parameters, %{}))
    |> driver_connect!(driver_params)
  end

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  def disconnect(port) do
    true = Driver.close(port)
    :ok
  end

  def load_tables(connection, table) do
    case Driver.execute(connection, "SELECT * FROM #{table.db_name} LIMIT 0") do
      :ok ->
        case Driver.get_columns(connection) do
          {:ok, []} ->
            DataSource.raise_error("Table #{table.db_name} does not have any columns")

          {:ok, columns} ->
            columns = Enum.map(columns, fn {name, type_name} -> Table.column(name, parse_type(type_name)) end)
            [%{table | columns: columns}]

          {:error, reason} ->
            DataSource.raise_error("`#{to_string(reason)}`")
        end

      {:error, reason} ->
        DataSource.raise_error("`#{to_string(reason)}`")
    end
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

  defp driver_connect!(conn_params, driver_params) do
    port = Driver.open()

    with :ok <- Driver.connect(port, to_connection_string(conn_params)) do
      if Keyword.get(driver_params, :wstr_as_bin), do: :ok = Driver.set_wstr_as_bin(port)
      port
    else
      {:error, reason} ->
        DataSource.raise_error(
          "Failed to establish a connection to the database. " <>
            "Please check that the database server is running, is reachable from the " <>
            "Insights Cloak host, and the database credentials are correct. " <>
            "The database driver reported the following exception: `#{to_string(reason)}`"
        )
    end
  end

  defp to_connection_string(parameters) do
    parameters
    |> Enum.map(fn {key, value} ->
      if value |> to_string() |> String.contains?([";", "{"]),
        do: DataSource.raise_error("The characters ';' and '{' are not allowed inside ODBC driver parameters!")

      "#{Atom.to_string(key)}=#{value}"
    end)
    |> Enum.join(";")
  end

  defp stream_rows(port, row_mapper) do
    Stream.resource(
      fn -> port end,
      fn port ->
        case Driver.fetch_batch(port, row_mapper, Cloak.DataSource.Driver.batch_size()) do
          {:ok, []} -> {:halt, port}
          {:ok, rows} -> {[rows], port}
          {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
        end
      end,
      fn _port -> :ok end
    )
  end

  defp map_fields([], []), do: []

  defp map_fields([field | rest_fields], [mapper | rest_mappers]),
    do: [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(:datetime), do: &datetime_field_mapper/1
  defp type_to_field_mapper(:time), do: &time_field_mapper/1
  defp type_to_field_mapper(:date), do: &date_field_mapper/1
  defp type_to_field_mapper(:real), do: &real_field_mapper/1
  defp type_to_field_mapper(:integer), do: &integer_field_mapper/1
  defp type_to_field_mapper(:interval), do: &interval_field_mapper(&1)
  defp type_to_field_mapper(:boolean), do: &boolean_field_mapper(&1)
  defp type_to_field_mapper(_), do: &generic_field_mapper/1

  defp generic_field_mapper(nil), do: nil
  defp generic_field_mapper(value), do: value

  defp datetime_field_mapper(nil), do: nil

  defp datetime_field_mapper(string) when is_binary(string), do: Cloak.Time.parse_datetime(string) |> error_to_nil()

  defp date_field_mapper(nil), do: nil

  defp date_field_mapper(string) when is_binary(string), do: Cloak.Time.parse_date(string) |> error_to_nil()

  defp time_field_mapper(nil), do: nil

  defp time_field_mapper(string) when is_binary(string), do: Cloak.Time.parse_time(string) |> error_to_nil()

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
    {value, _} = Integer.parse(value)
    value
  end

  defp integer_field_mapper(value) when is_integer(value), do: value
  defp integer_field_mapper(value) when is_float(value), do: round(value)

  defp interval_field_mapper(nil), do: nil

  defp interval_field_mapper(string) when is_binary(string),
    do: string |> String.to_integer() |> Timex.Duration.from_seconds()

  defp interval_field_mapper(number), do: Timex.Duration.from_seconds(number)

  defp boolean_field_mapper(0), do: false
  defp boolean_field_mapper(other) when is_integer(other), do: true
  defp boolean_field_mapper(nil), do: nil

  defp parse_type("varchar"), do: :text
  defp parse_type("wvarchar"), do: :text
  defp parse_type("binary"), do: :text
  defp parse_type("guid"), do: :text
  defp parse_type("bit"), do: :boolean
  defp parse_type("bigint"), do: :integer
  defp parse_type("integer"), do: :integer
  defp parse_type("float"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type("datetime"), do: :datetime
  defp parse_type("timestamp"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}
end
