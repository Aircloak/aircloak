defmodule Cloak.DataSource.Oracle do
  @moduledoc """
  Implements the DataSource.Driver behaviour for the Oracle Database, targeting version g11 R2.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  alias Cloak.DataSource.RODBC

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(), do: SqlBuilder.Oracle

  @impl Driver
  def connect(parameters), do: RODBC.connect(parameters, &conn_params/1)

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table) do
    # ODBC driver returns datetime for all date columns, so we need to query oracle directly to determine date columns
    date_columns = date_columns(connection, table)

    # ODBC driver returns float for all numeric columns, so we need to query oracle directly to determine date columns
    integer_columns = integer_columns(connection, table)

    # Once we've figured out which columns are dates, we'll delegate to RODBC to get all the column types, and manually
    # set the `:date` type for date columns.
    columns =
      connection
      |> RODBC.table_columns(update_in(table.db_name, &"\"#{&1}\""))
      |> Enum.map(fn column ->
        if MapSet.member?(date_columns, column.name), do: %{column | type: :date}, else: column
      end)
      |> Enum.map(fn column ->
        if MapSet.member?(integer_columns, column.name), do: %{column | type: :integer}, else: column
      end)

    [%{table | columns: columns}]
  end

  @impl Driver
  def select(connection, sql_query, result_processor),
    do: RODBC.select(connection, sql_query, custom_mappers(), result_processor)

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  @impl Driver
  def supports_query?(query), do: query.limit == nil and query.offset == 0

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    hostname = normalized_parameters.hostname
    port = Map.get(normalized_parameters, :port, 1521)
    database = normalized_parameters.database

    %{
      DBQ: "#{hostname}:#{port}/#{database}",
      Uid: normalized_parameters[:username],
      Pwd: normalized_parameters[:password],
      DSN: "Oracle"
    }
  end

  defp date_columns(connection, table) do
    statement = ~s/
      SELECT COLUMN_NAME
      FROM ALL_TAB_COLUMNS
      WHERE TABLE_NAME = '#{table.db_name}' AND DATA_TYPE = 'DATE'
    /
    {:ok, rows} = RODBC.select_direct(connection, statement)

    rows
    |> Stream.map(fn [name] -> name end)
    |> MapSet.new()
  end

  defp integer_columns(connection, table) do
    statement = ~s/
      SELECT COLUMN_NAME
      FROM ALL_TAB_COLUMNS
      WHERE TABLE_NAME = '#{table.db_name}' AND DATA_TYPE = 'NUMBER' and COALESCE(DATA_SCALE, 0) = 0
    /
    {:ok, rows} = RODBC.select_direct(connection, statement)

    rows
    |> Stream.map(fn [name] -> name end)
    |> MapSet.new()
  end

  defp custom_mappers() do
    %{
      :text => &text_mapper/1,
      :date => &date_mapper/1,
      :datetime => &datetime_mapper/1
    }
  end

  # In Oracle, NULL and empty string are the same thing (e.g. `select coalesce(trim('  '), 'is null') from dual` returns
  # 'is null'). Therefore, we're converting NULL into an empty string.
  defp text_mapper(nil), do: ""
  defp text_mapper(value), do: value

  defp datetime_mapper(string) do
    string
    # In some cases (e.g. when data type is timestamp(0)), we can get a trailing `.` which we need to remove.
    |> String.split(~r/\.$/)
    |> hd()
    |> Cloak.Time.parse_datetime()
    |> case do
      {:ok, datetime} -> datetime
      {:error, _reason} -> nil
    end
  end

  defp date_mapper(string) do
    case Cloak.Time.parse_datetime(string) do
      {:ok, datetime} -> NaiveDateTime.to_date(datetime)
      {:error, _reason} -> nil
    end
  end
end
