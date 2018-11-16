defmodule Cloak.DataSource.Oracle do
  @moduledoc """
  Implements the DataSource.Driver behaviour for the Oracle Database, targeting version g11 R2.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL

  alias Cloak.DataSource
  alias Cloak.DataSource.{SqlBuilder, Table}

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(), do: SqlBuilder.Oracle

  @impl Driver
  def connect(parameters) do
    :jamdb_oracle.start_link(
      host: to_charlist(parameters.hostname),
      port: parameters[:port],
      user: to_charlist(parameters.username),
      password: to_charlist(parameters.password),
      sid: to_charlist(parameters.sid)
    )
    |> case do
      {:ok, conn} -> {:ok, conn}
      error -> {:error, inspect(error)}
    end
  end

  @impl Driver
  def disconnect(connection), do: :jamdb_oracle.stop(connection)

  @impl Driver
  def load_tables(connection, table) do
    """
      SELECT column_name, data_type
      FROM all_tab_columns
      WHERE table_name = '#{table.db_name |> String.replace("'", "''")}'
    """
    |> run_query(connection)
    |> case do
      {:ok, []} -> DataSource.raise_error("Table `#{table.db_name}` does not exist")
      {:ok, rows} -> [%{table | columns: Enum.map(rows, &build_column/1)}]
      {:error, reason} -> DataSource.raise_error("`#{reason}`")
    end
  end

  @impl Driver
  def select(connection, sql_query, result_processor) do
    field_mappers = Enum.map(sql_query.db_columns, &type_to_field_mapper(&1.type))

    SqlBuilder.build(sql_query)
    |> run_query(connection)
    |> case do
      {:ok, rows} -> {:ok, result_processor.([unpack(rows, field_mappers)])}
      {:error, reason} -> {:error, reason}
    end
  end

  @impl Driver
  def driver_info(_connection), do: nil

  @impl Driver
  def supports_query?(query), do: query.limit == nil and query.offset == 0

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp type_to_field_mapper(:text), do: &to_string/1
  defp type_to_field_mapper(:integer), do: fn {x} -> x end
  defp type_to_field_mapper(:real), do: fn {x} -> x end
  defp type_to_field_mapper(:boolean), do: fn {x} -> x != 0 end

  defp type_to_field_mapper(:datetime),
    do: fn datetime -> datetime |> NaiveDateTime.from_erl!() |> Cloak.Time.max_precision() end

  defp type_to_field_mapper(:date),
    do: fn {date, _time} -> Date.from_erl!(date) end

  defp unpack(rows, field_mappers), do: Stream.map(rows, &map_fields(&1, field_mappers))

  defp map_fields([], []), do: []

  defp map_fields([field | rest_fields], [mapper | rest_mappers]),
    do: [mapper.(field) | map_fields(rest_fields, rest_mappers)]

  defp run_query(query, connection) do
    case :jamdb_oracle.sql_query(connection, to_charlist(query)) do
      {:ok, [{:result_set, _columns, _, rows}]} -> {:ok, rows}
      {:ok, [{:proc_result, _code, text}]} -> {:error, to_string(text)}
      other -> {:error, inspect(other)}
    end
  end

  defp build_column([name, type]), do: Table.column(to_string(name), type |> to_string() |> parse_type())

  defp parse_type("DATE"), do: :date
  defp parse_type("TIMESTAMP" <> _), do: :datetime
  defp parse_type("BINARY_FLOAT"), do: :real
  defp parse_type("BINARY_DOUBLE"), do: :real
  defp parse_type("NUMBER" <> rest), do: if(rest =~ ~r/\(.*,.*\)/, do: :real, else: :integer)
  defp parse_type("VARCHAR" <> _), do: :text
  defp parse_type("VARCHAR2" <> _), do: :text
  defp parse_type(other), do: {:unsupported, other}
end
