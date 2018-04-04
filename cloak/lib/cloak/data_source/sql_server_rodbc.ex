defmodule Cloak.DataSource.SQLServerRODBC do
  @moduledoc "Implements the DataSource.Driver behaviour for MS SQL Server. For more information, see `DataSource`."

  alias Cloak.DataSource.{Table, RODBC}
  alias Cloak.DataSource

  use Cloak.DataSource.Driver.SQL

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect!(parameters) do
    normalized_parameters =
      for {key, value} <- parameters,
          into: %{},
          do: {key |> Atom.to_string() |> String.downcase() |> String.to_atom(), value}

    odbc_parameters =
      %{
        DSN: "SQLServer",
        Server: normalized_parameters[:hostname],
        Uid: normalized_parameters[:username],
        Pwd: normalized_parameters[:password],
        Database: normalized_parameters[:database]
      }
      |> add_optional_parameters(parameters)

    connection = RODBC.connect!(odbc_parameters)
    :ok = RODBC.Driver.execute(connection, "SET ANSI_DEFAULTS ON")
    connection
  end

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table) do
    {schema_name, table_name} =
      case String.split(table.db_name, ".") do
        [full_table_name] -> {"dbo", full_table_name}
        [schema_name, table_name] -> {schema_name, table_name}
      end

    statement =
      "SELECT column_name, data_type FROM information_schema.columns " <>
        "WHERE table_name = '#{table_name}' AND table_schema = '#{schema_name}' ORDER BY ordinal_position DESC"

    row_mapper = fn [name, type_name] -> Table.column(name, parse_type(type_name)) end

    case RODBC.Driver.execute(connection, statement) do
      :ok ->
        case RODBC.Driver.fetch_all(connection, row_mapper) do
          {:ok, []} -> DataSource.raise_error("Table `#{table.db_name}` does not exist")
          {:ok, columns} -> [%{table | columns: Enum.to_list(columns)}]
          {:error, reason} -> DataSource.raise_error("`#{to_string(reason)}`")
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

  @impl Driver
  def sql_dialect_module(_parameters), do: SqlBuilder.SQLServer

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # Allows for adding additional ODBC connection parameters in the case where
  # a SQL Server installation requires additional parameters.
  defp add_optional_parameters(default_params, %{odbc_parameters: additonal_parameters}),
    do: Map.merge(default_params, additonal_parameters)

  defp add_optional_parameters(default_params, _), do: default_params

  defp parse_type("varchar"), do: :text
  defp parse_type("char"), do: :text
  defp parse_type("nchar"), do: :text
  defp parse_type("nvarchar"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("ntext"), do: :text
  defp parse_type("bit"), do: :boolean
  defp parse_type("int"), do: :integer
  defp parse_type("bigint"), do: :integer
  defp parse_type("smallint"), do: :integer
  defp parse_type("tinyint"), do: :integer
  defp parse_type("real"), do: :real
  defp parse_type("float"), do: :real
  defp parse_type("money"), do: :real
  defp parse_type("smallmoney"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("uniqueidentifier"), do: :text
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type("datetime"), do: :datetime
  defp parse_type("datetime2"), do: :datetime
  defp parse_type("smalldatetime"), do: :datetime
  defp parse_type("datetimeoffset"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}
end
