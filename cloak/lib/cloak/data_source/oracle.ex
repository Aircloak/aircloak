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
    run_query(connection, """
      SELECT column_name, data_type
      FROM all_tab_columns
      WHERE upper(table_name) = '#{table.db_name |> String.upcase() |> String.replace("'", "''")}'
    """)
    |> case do
      {:ok, []} -> DataSource.raise_error("Table `#{table.db_name}` does not exist")
      {:ok, rows} -> [%{table | columns: Enum.map(rows, &build_column/1)}]
      {:error, reason} -> DataSource.raise_error("`#{reason}`")
    end
  end

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: Cloak.DataSource.RODBC

  @impl Driver
  defdelegate driver_info(connection), to: Cloak.DataSource.RODBC

  @impl Driver
  def supports_query?(query),
    do: query |> get_in([Cloak.Sql.Query.Lenses.joins()]) |> Enum.any?(&(&1.type == :cross_join)) |> :erlang.not()

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_query(connection, query) do
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
