defmodule Cloak.DataSource.Oracle do
  @moduledoc """
  Implements the DataSource.Driver behaviour for the Oracle Database, targeting version g11 R2.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  alias Cloak.DataSource.RODBC
  alias Cloak.Sql.Expression

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
    columns =
      connection
      |> RODBC.table_columns(update_in(table.db_name, &SqlBuilder.quote_table_name/1))
      |> fix_column_types(connection, table)

    [%{table | columns: columns}]
  end

  @impl Driver
  def select(connection, sql_query, result_processor),
    do: RODBC.select(connection, map_selected_expressions(sql_query), custom_mappers(), result_processor)

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  @impl Driver
  def supports_query?(query), do: query.limit == nil and query.offset == 0

  @impl Driver
  def supports_materialized_views?(), do: true

  @impl Driver
  def store_materialized_view(connection, name, query) do
    table_name = SqlBuilder.quote_table_name(name)
    sql = "CREATE TABLE #{table_name} AS #{SqlBuilder.build(query)}"
    with {:ok, _} = RODBC.select_direct(connection, sql), do: :ok
  end

  @impl Driver
  def materialized_views(connection) do
    {:ok, rows} = RODBC.select_direct(connection, "select table_name from user_tables where table_name like '__ac_%'")
    Enum.map(rows, fn [table_name] -> table_name end)
  end

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

  defp fix_column_types(rodbc_columns, connection, table) do
    # Oracle ODBC driver returns wrong information for some data types (see below). Therefore, we'll query the
    # metadata table ALL_TAB_COLUMNS to figure out the correct types.
    correct_column_types =
      %{
        # Oracle ODBC driver returns datetime for date type
        date: "DATA_TYPE = 'DATE'",
        # Oracle ODBC driver returns float for `NUMBER` type, so we need to check the scale to recognize integers
        integer: "DATA_TYPE = 'NUMBER' and DATA_SCALE = 0"
      }
      |> Stream.flat_map(fn {type, filter} ->
        statement = "SELECT COLUMN_NAME FROM ALL_TAB_COLUMNS WHERE #{fix_column_types_filter(table, filter)}"
        {:ok, rows} = RODBC.select_direct(connection, statement)
        Enum.map(rows, fn [name] -> {name, type} end)
      end)
      |> Map.new()

    Enum.map(rodbc_columns, &%{&1 | type: Map.get(correct_column_types, &1.name, &1.type)})
  end

  defp fix_column_types_filter(table, filter) do
    table_filters =
      case SqlBuilder.table_name_parts(table.db_name) do
        [table_name] -> ["TABLE_NAME = '#{table_name}'"]
        [schema_name, table_name] -> ["OWNER = '#{schema_name}'", "TABLE_NAME = '#{table_name}'"]
      end

    Enum.join([filter | table_filters], " AND ")
  end

  # We need to perform explicit casting on some selected types, because ODBC driver can't handle them.
  defp map_selected_expressions(sql_query),
    do: %Cloak.Sql.Query{sql_query | db_columns: Enum.map(sql_query.db_columns, &map_selected_expression/1)}

  defp map_selected_expression(%Expression{type: :time} = expression),
    do: Expression.function({:cast, {:native_type, "TIMESTAMP"}}, [expression], :time)

  defp map_selected_expression(%Expression{type: :interval} = expression),
    do: Expression.function("TO_CHAR", [expression], :interval)

  defp map_selected_expression(%Expression{} = expression), do: expression

  defp custom_mappers() do
    %{
      :text => &text_mapper/1,
      :date => nil_mapper(&date_mapper/1),
      :time => nil_mapper(&time_mapper/1),
      :datetime => nil_mapper(&datetime_mapper/1),
      :interval => nil_mapper(&interval_mapper/1),
      :boolean => nil_mapper(&boolean_mapper/1)
    }
  end

  @doc false
  def nil_mapper(mapper) do
    fn
      nil -> nil
      other -> mapper.(other)
    end
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

  defp time_mapper(string) do
    with datetime when not is_nil(datetime) <- datetime_mapper(string), do: NaiveDateTime.to_time(datetime)
  end

  defp boolean_mapper(value), do: round(value) != 0

  @doc false
  def interval_mapper(string) do
    import Combine.Parsers.{Base, Text}

    # Parsing of Oracle interval string format. The interval is given as SDDDDDDDDD HH:MM:SS.FFFFFFFFF
    #   S - sign (+ or -)
    #   D - number of days
    #   H - hours
    #   M - minutes
    #   S - seconds
    #   F - fractions of seconds
    case Combine.parse(
           string,
           sequence([
             map(either(char(?-), char(?+)), &Map.fetch!(%{"-" => -1, "+" => +1}, &1)),
             integer(),
             ignore(space()),
             integer(),
             ignore(char(?:)),
             integer(),
             ignore(char(?:)),
             integer(),
             ignore(char(?.)),
             ignore(integer())
           ])
         ) do
      [[sign, days, hours, minutes, seconds]] ->
        Timex.Duration.from_seconds(sign * (((days * 24 + hours) * 60 + minutes) * 60 + seconds))

      {:error, _reason} ->
        nil
    end
  end
end
