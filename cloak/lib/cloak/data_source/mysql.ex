defmodule Cloak.DataSource.MySQL do
  @moduledoc """
  Implements the DataSource.Driver behaviour for MySQL / MariaDB.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.SqlBuilder

  #-----------------------------------------------------------------------------------------------------------
  # DataSource.Driver callbacks
  #-----------------------------------------------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @doc false
  def connect!(parameters) do
    parameters =
      Enum.to_list(parameters) ++
      [types: true, sync_connect: true, pool: DBConnection.Connection, timeout: :timer.hours(2)]
    {:ok, connection} = Mariaex.start_link(parameters)
    {:ok, %Mariaex.Result{}} = Mariaex.query(connection, "SET sql_mode = 'ANSI,NO_BACKSLASH_ESCAPES'", [])
    connection
  end
  @doc false
  def disconnect(connection) do
    GenServer.stop(connection)
  end

  @doc false
  def load_tables(connection, table) do
    query = "SHOW COLUMNS FROM #{table.db_name}"
    column_info_mapper = fn [name, type | _others] -> {name, parse_type(type)} end
    {:ok, columns} = run_query(connection, query, column_info_mapper, &Enum.to_list/1)
    [%{table | columns: columns}]
  end

  @doc false
  def select(connection, aql_query, result_processor) do
    statement = SqlBuilder.build(aql_query, :mysql)
    run_query(connection, statement, &row_mapper/1, result_processor)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_query(connection, statement, decode_mapper, result_processor) do
    options = [decode_mapper: decode_mapper, timeout: :timer.hours(2)]
    with {:ok, %Mariaex.Result{rows: rows}} <- Mariaex.query(connection, statement, [], options) do
      {:ok, result_processor.(rows)}
    end
  end

  defp parse_type("varchar" <> _size), do: :text
  defp parse_type("char" <> _size), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("bit(1)"), do: :boolean
  defp parse_type("int" <> _size), do: :integer
  defp parse_type("tinyint" <> _size), do: :integer
  defp parse_type("mediumint" <> _size), do: :integer
  defp parse_type("bigint" <> _size), do: :integer
  defp parse_type("float"), do: :real
  defp parse_type("double"), do: :real
  defp parse_type("decimal" <> _size), do: :real
  defp parse_type("timestamp"), do: :datetime
  defp parse_type("datetime"), do: :datetime
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type(type), do: {:unsupported, type}


  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp row_mapper(row), do: for field <- row, do: field_mapper(field)

  defp field_mapper({{year, month, day}, {hour, min, sec, msec}}), do:
    NaiveDateTime.new(year, month, day, hour, min, sec, {msec * 1000, 6}) |> error_to_nil()
  defp field_mapper({year, month, day}), do: Date.new(year, month, day) |> error_to_nil()
  defp field_mapper({hour, min, sec, msec}), do: Time.new(hour, min, sec, {msec * 1000, 6}) |> error_to_nil()
  defp field_mapper(<<0>>), do: false
  defp field_mapper(<<1>>), do: true
  @decimal_precision :math.pow(10, 15)
  defp field_mapper(%Decimal{} = value), do:
    Cloak.DecimalUtil.to_precision(value, @decimal_precision)
  defp field_mapper(field), do: field

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil


  #-----------------------------------------------------------------------------------------------------------
  # Test functions
  #-----------------------------------------------------------------------------------------------------------

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
