defmodule Cloak.DataSource.SAPIQ do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP IQ.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.SQL
  alias Cloak.DataSource
  alias Cloak.DataSource.{RODBC, Table}

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_), do: Cloak.DataSource.SqlBuilder.SAPIQ

  @impl Driver
  def connect!(parameters), do: RODBC.connect!(parameters, &conn_params/1)

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table) do
    statement = "SELECT cname, coltype FROM  sys.syscolumns WHERE tname='#{table.db_name}' ORDER BY colno DESC"

    row_mapper = fn [name, type_name] -> Table.column(name, parse_type(type_name)) end

    case RODBC.Driver.execute(connection, statement) do
      :ok ->
        case RODBC.Driver.fetch_all(connection, row_mapper) do
          {:ok, []} -> DataSource.raise_error("Table `#{table.db_name}` does not exist")
          {:ok, columns} -> [%{table | columns: Enum.to_list(columns), db_name: ~s/"#{table.db_name}"/}]
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

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp conn_params(normalized_parameters) do
    %{
      Host: "#{normalized_parameters[:hostname]}:#{normalized_parameters[:port]}",
      UserID: normalized_parameters[:username],
      Password: normalized_parameters[:password],
      DatabaseName: normalized_parameters[:database],
      DSN: "SAPIQ"
    }
  end

  defp parse_type("varchar"), do: :text
  defp parse_type("nvarchar"), do: :text
  defp parse_type("text"), do: :text
  defp parse_type("varbinary"), do: :text
  defp parse_type("binary"), do: :text
  defp parse_type("bit"), do: :boolean
  defp parse_type("bigint"), do: :integer
  defp parse_type("integer"), do: :integer
  defp parse_type("smallint"), do: :integer
  defp parse_type("tinyint"), do: :integer
  defp parse_type("float"), do: :real
  defp parse_type("double"), do: :real
  defp parse_type("numeric"), do: :real
  defp parse_type("decimal"), do: :real
  defp parse_type("real"), do: :real
  defp parse_type("time"), do: :time
  defp parse_type("date"), do: :date
  defp parse_type("smalldatetime"), do: :datetime
  defp parse_type("datetime"), do: :datetime
  defp parse_type("timestamp"), do: :datetime
  defp parse_type(type), do: {:unsupported, type}
end
