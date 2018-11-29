defmodule Cloak.DataSource.RODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for ODBC compatible data-stores, using the Rust port.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{RODBC.Port, SqlBuilder, Table, Driver, Parameters}
  alias Cloak.DataSource

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Normalizes the connection parameters and connects to the data source via odbc."
  @spec connect(Driver.parameters(), (map -> map), Keyword.t()) :: {:ok, pid()} | {:error, String.t()}
  def connect(parameters, conn_params_extractor, driver_params \\ []) do
    normalized_parameters = normalize_parameters(parameters)

    normalized_parameters
    |> conn_params_extractor.()
    |> Map.merge(Map.get(normalized_parameters, :odbc_parameters, %{}))
    |> driver_connect(driver_params)
  end

  @doc "Closes the connection"
  @spec disconnect(pid) :: :ok
  def disconnect(connection), do: Port.stop(connection)

  @doc "Loads one or more table definitions from the database."
  @spec load_tables(pid, Table.t()) :: [Table.t()]
  def load_tables(connection, table, table_load_statement \\ &"SELECT * FROM #{&1} WHERE 0 = 1"),
    do: [%{table | columns: table_columns(connection, table, table_load_statement)}]

  @doc "Returns the list of columns for the given table."
  @spec table_columns(pid, Table.t()) :: [Table.column()]
  def table_columns(connection, table, table_load_statement \\ &"SELECT * FROM #{&1} WHERE 0 = 1") do
    case Port.execute(connection, table_load_statement.(table.db_name), Driver.timeout()) do
      :ok ->
        case Port.get_columns(connection) do
          {:ok, []} -> DataSource.raise_error("Table #{table.db_name} does not have any columns")
          {:ok, columns} -> Enum.map(columns, fn {name, type_name} -> Table.column(name, parse_type(type_name)) end)
          {:error, reason} -> DataSource.raise_error("`#{to_string(reason)}`")
        end

      {:error, reason} ->
        DataSource.raise_error("`#{to_string(reason)}`")
    end
  end

  @doc "Selects the data from the database."
  @spec select(pid, Cloak.Sql.Query.t(), %{Table.data_type() => (term -> term)}, DataSource.result_processor()) ::
          {:ok, DataSource.processed_result()} | {:error, any}
  def select(connection, sql_query, driver_mappers \\ %{}, result_processor) do
    statement = SqlBuilder.build(sql_query)
    field_mappers = Enum.map(sql_query.db_columns, &type_to_field_mapper(driver_mappers, &1.type))
    row_mapper = &map_fields(&1, field_mappers)

    case Port.execute(connection, statement, Driver.timeout()) do
      :ok -> {:ok, connection |> stream_rows(row_mapper) |> result_processor.()}
      {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
    end
  end

  @doc "Executes a raw SQL SELECT statement on the given connection."
  @spec select_direct(pid, String.t()) :: Enumerable.t()
  def select_direct(connection, statement) do
    with :ok <- Port.execute(connection, statement, Driver.timeout()),
         do: {:ok, Stream.concat(stream_rows(connection, & &1))}
  end

  @doc "Returns the driver specific information to be stored inside the data source structure."
  @spec driver_info(pid) :: nil
  def driver_info(_connection), do: nil

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp driver_connect(conn_params, driver_params) do
    {:ok, connection} = Port.start_link()

    if Keyword.get(driver_params, :wstr_as_utf16), do: :ok = Port.set_wstr_as_utf16(connection)

    with :ok <- Port.connect(connection, to_connection_string(conn_params), Driver.connect_timeout()),
         do: {:ok, connection}
  end

  defp to_connection_string(parameters) do
    parameters
    |> Enum.map(fn {key, value} ->
      "#{Atom.to_string(key)}=#{Parameters.odbc_escape(value)}"
    end)
    |> Enum.join(";")
  end

  defp normalize_parameters(parameters) do
    parameters
    |> Stream.map(fn {key, value} -> {downcase_key(key), value} end)
    |> Stream.reject(fn {key, _value} -> is_nil(key) end)
    |> Enum.into(%{})
  end

  defp downcase_key(key) do
    string_key = key |> Atom.to_string() |> String.downcase()

    try do
      String.to_existing_atom(string_key)
    rescue
      ArgumentError -> nil
    end
  end

  defp stream_rows(connection, row_mapper) do
    Stream.resource(
      fn -> connection end,
      fn connection ->
        case Port.fetch_batch(connection, row_mapper, Driver.batch_size()) do
          {:ok, []} -> {:halt, connection}
          {:ok, rows} -> {[rows], connection}
          {:error, reason} -> DataSource.raise_error("Driver exception: `#{to_string(reason)}`")
        end
      end,
      fn _port -> :ok end
    )
  end

  defp map_fields([], []), do: []

  defp map_fields([field | rest_fields], [mapper | rest_mappers]),
    do: [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp type_to_field_mapper(driver_mappers, type) do
    default_mappers = %{
      :datetime => &datetime_field_mapper/1,
      :time => &time_field_mapper/1,
      :date => &date_field_mapper/1,
      :real => &real_field_mapper/1,
      :integer => &integer_field_mapper/1,
      :interval => &interval_field_mapper(&1),
      :boolean => &boolean_field_mapper(&1)
    }

    {:ok, mapper} =
      with :error <- Map.fetch(driver_mappers, type),
           :error <- Map.fetch(default_mappers, type),
           do: {:ok, &generic_field_mapper/1}

    mapper
  end

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
  defp parse_type("nvarchar"), do: :text
  defp parse_type("nclob"), do: :text
  defp parse_type("clob"), do: :text
  defp parse_type("blob"), do: :text
  defp parse_type("varbinary"), do: :text
  defp parse_type("alphanumeric"), do: :text
  defp parse_type("boolean"), do: :boolean
  defp parse_type("smallint"), do: :integer
  defp parse_type("tinyint"), do: :integer
  defp parse_type("real"), do: :real
  defp parse_type("double"), do: :real
  defp parse_type("decimal" <> _), do: :real
  defp parse_type("smalldecimal"), do: :real
  defp parse_type("seconddate"), do: :datetime
  defp parse_type("text"), do: :text
  defp parse_type("decimal"), do: :real
  defp parse_type("smalldatetime"), do: :datetime
  defp parse_type("char"), do: :text
  defp parse_type("nchar"), do: :text
  defp parse_type("ntext"), do: :text
  defp parse_type("int"), do: :integer
  defp parse_type("money"), do: :real
  defp parse_type("smallmoney"), do: :real
  defp parse_type("uniqueidentifier"), do: :text
  defp parse_type("datetime2"), do: :datetime
  defp parse_type("datetimeoffset"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}
end
