defmodule Cloak.DataSource.Oracle do
  @moduledoc """
  Implements the DataSource.Driver behaviour for the Oracle Database, targeting version 12c.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.RodbcSql
  use Cloak.DataSource.Driver.SQL.AnalystTables
  require Logger
  alias Cloak.DataSource.{RODBC, Table, SqlBuilder}
  alias Cloak.Sql.Expression
  alias Cloak.Query.ExecutionError

  @mathematical_operators ~w(+ - * / ^)

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters), do: RODBC.connect(parameters, &conn_params/1)

  @impl Driver
  def health_check(connection) do
    case select(connection, "SELECT 1 FROM DUAL") do
      {:ok, _} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @impl Driver
  def load_tables(connection, table) do
    connection
    |> select!("""
      SELECT COLUMN_NAME, DATA_TYPE, DATA_SCALE
      FROM ALL_TAB_COLUMNS
      WHERE #{table_filter(table)}
    """)
    |> Enum.map(fn
      [column_name, "NUMBER", scale] when scale == nil or scale <= 0 -> Table.column(column_name, :integer)
      [column_name, data_type, _] -> Table.column(column_name, RODBC.parse_column_type(data_type))
    end)
    |> case do
      [] -> raise ExecutionError, message: "Table #{table.db_name} does not have any columns"
      columns -> [load_comments(connection, %{table | columns: columns})]
    end
  end

  @impl Driver
  def select(connection, query, result_processor) do
    query = map_selected_expressions(query)
    statement = query |> SqlBuilder.build() |> insert_select_hints(query.data_source.parameters[:select_hints])
    RODBC.select(connection, statement, query.db_columns, custom_mappers(), result_processor)
  end

  @impl Driver
  def supports_function?(expression = %Expression{kind: :function, name: name}, data_source) do
    if name in @mathematical_operators do
      Map.get(data_source[:parameters], :aircloak_udfs, false)
    else
      SqlBuilder.Support.supports_function?(expression, data_source)
    end
  end

  @impl Driver
  def supports_analyst_tables?(), do: true

  @impl Driver
  def prepare_analyst_table(table_name, query) do
    quoted_table_name = SqlBuilder.quote_table_name(table_name)

    select_statement =
      query
      |> SqlBuilder.build()
      |> insert_select_hints(query.data_source.parameters[:select_hints])

    "CREATE TABLE #{quoted_table_name} AS #{select_statement}"
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

  defp load_comments(connection, table) do
    [table_comments] =
      connection
      |> select!("""
        SELECT COMMENTS
        FROM ALL_TAB_COMMENTS
        WHERE #{table_filter(table)}
      """)
      |> Enum.at(0, [nil])

    column_comments =
      connection
      |> select!("""
        SELECT COLUMN_NAME, COMMENTS
        FROM ALL_COL_COMMENTS
        WHERE #{table_filter(table)} AND COMMENTS IS NOT NULL
      """)
      |> Enum.map(&List.to_tuple/1)
      |> Enum.into(%{})

    comments =
      %{table: table_comments, columns: column_comments}
      |> Aircloak.deep_merge(Map.get(table, :comments, %{}))

    Map.put(table, :comments, comments)
  end

  defp table_filter(table) do
    case SqlBuilder.table_name_parts(table.db_name) do
      [table_name] -> "TABLE_NAME = '#{table_name}'"
      [schema_name, table_name] -> "OWNER = '#{schema_name}' AND TABLE_NAME = '#{table_name}'"
    end
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

  defp insert_select_hints(statement, select_hints) when select_hints in [nil, ""], do: statement

  defp insert_select_hints("SELECT " <> statement_body, select_hints), do: "SELECT #{select_hints} #{statement_body}"
end
