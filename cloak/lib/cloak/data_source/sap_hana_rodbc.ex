defmodule Cloak.DataSource.SAPHanaRODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP HANA using the Rust ODBC port driver.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{Table, RODBC}
  alias Cloak.DataSource

  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_), do: Cloak.DataSource.SqlBuilder.SAPHana

  @impl Driver
  def connect!(parameters) do
    unless File.exists?(Cloak.SapHanaHelpers.driver_path()),
      do: DataSource.raise_error("ODBC driver for SAP HANA is not mounted.")

    RODBC.connect!(parameters, &conn_params/1)
  end

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table) do
    statement =
      "SELECT column_name, lower(data_type_name) FROM table_columns " <>
        "WHERE table_name = '#{table.db_name}' AND schema_name in (SELECT CURRENT_SCHEMA FROM DUMMY) " <>
        "ORDER BY position DESC"

    row_mapper = fn [name, type_name] -> Table.column(name, parse_type(type_name)) end

    case RODBC.Driver.execute(connection, statement) do
      :ok ->
        case RODBC.Driver.fetch_all(connection, row_mapper) do
          {:ok, []} ->
            DataSource.raise_error("Table `#{table.db_name}` does not exist")

          {:ok, columns} ->
            [%{table | columns: Enum.to_list(columns), db_name: ~s/"#{table.db_name}"/}]

          {:error, reason} ->
            DataSource.raise_error("`#{to_string(reason)}`")
        end

      {:error, reason} ->
        DataSource.raise_error("`#{to_string(reason)}`")
    end
  end

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: RODBC

  @impl Driver
  defdelegate driver_info(connection), to: RODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: RODBC

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    %{
      servernode: "#{normalized_parameters[:hostname]}:#{normalized_parameters[:port]}",
      Uid: normalized_parameters[:username],
      Pwd: normalized_parameters[:password],
      databasename: normalized_parameters[:database],
      DSN: "SAPHana"
    }
    |> Map.merge(schema_option(Cloak.DataSource.SAPHana.default_schema()))
  end

  defp schema_option(nil), do: %{}
  defp schema_option(schema), do: %{cs: ~s/"#{schema}"/}

  defp parse_type("varchar"), do: :text
  defp parse_type("nvarchar"), do: :text
  defp parse_type("nclob"), do: :text
  defp parse_type("clob"), do: :text
  defp parse_type("blob"), do: :text
  defp parse_type("varbinary"), do: :text
  defp parse_type("binary"), do: :text
  defp parse_type("alphanumeric"), do: :text
  defp parse_type("boolean"), do: :boolean
  defp parse_type("bigint"), do: :integer
  defp parse_type("integer"), do: :integer
  defp parse_type("smallint"), do: :integer
  defp parse_type("tinyint"), do: :integer
  defp parse_type("real"), do: :real
  defp parse_type("double"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("decimal" <> _), do: :real
  defp parse_type("smalldecimal"), do: :real
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type("seconddate"), do: :datetime
  defp parse_type("timestamp"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}
end
