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
  def connect(parameters) do
    parameters = Enum.to_list(parameters) ++ [types: true, sync_connect: true, pool: DBConnection.Connection]
    with {:ok, connection} = Mariaex.start_link(parameters) do
      {:ok, %Mariaex.Result{}} = Mariaex.query(connection, "SET sql_mode = 'ANSI,NO_BACKSLASH_ESCAPES'", [])
      {:ok, connection}
    end
  end
  @doc false
  def disconnect(connection) do
    GenServer.stop(connection)
  end

  @doc false
  def describe_table(connection, table_name) do
    query = "SHOW COLUMNS FROM #{table_name}"
    row_mapper = fn [name, type | _others] -> {name, parse_type(type)} end
    {:ok, columns_list} = run_query(connection, query, row_mapper, &Enum.to_list/1)
    columns_list
  end

  @doc false
  def select(connection, aql_query, result_processor) do
    statement = SqlBuilder.build(:mysql, aql_query)
    run_query(connection, statement, &row_mapper/1, result_processor)
  end


  #-----------------------------------------------------------------------------------------------------------
  # Internal functions
  #-----------------------------------------------------------------------------------------------------------

  defp run_query(connection, statement, decode_mapper, result_processor) do
    options = [decode_mapper: decode_mapper, timeout: :timer.hours(4)]
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
  defp parse_type("timestamp"), do: :timestamp
  defp parse_type("datetime"), do: :timestamp
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type(type), do: {:unsupported, type}


  # -------------------------------------------------------------------
  # Selected data mapping functions
  # -------------------------------------------------------------------

  defp row_mapper(row), do: for field <- row, do: field_mapper(field)

  defp field_mapper({{year, month, day}, {hour, min, sec, msec}}), do:
    NaiveDateTime.new(year, month, day, hour, min, sec, msec * 1000) |> error_to_nil()
  defp field_mapper({year, month, day}), do:
    Date.new(year, month, day) |> error_to_nil()
  defp field_mapper({hour, min, sec, msec}), do:
    Time.new(hour, min, sec, msec * 1000) |> error_to_nil()
  defp field_mapper(<<0>>), do: false
  defp field_mapper(<<1>>), do: true
  defp field_mapper(field), do: field

  defp error_to_nil({:ok, result}), do: result
  defp error_to_nil({:error, _reason}), do: nil
end
