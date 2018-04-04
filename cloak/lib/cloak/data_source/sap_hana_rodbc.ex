defmodule Cloak.DataSource.SAPHanaRODBC do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP HANA using the Rust ODBC port driver.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.{Table, RODBC}
  alias Cloak.DataSource

  use Cloak.DataSource.Driver.SQL

  @doc """
  Returns the SAP HANA schema as configured in app config.

  This is useful in development, to allow different developers to work on different schemas.
  """
  @spec default_schema() :: nil | String.t()
  def default_schema(),
    do: non_empty_schema(default_schema_from_os_env()) || non_empty_schema(default_schema_from_app_config())

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def sql_dialect_module(_), do: Cloak.DataSource.SqlBuilder.SAPHana

  @impl Driver
  def connect!(parameters) do
    normalized_parameters =
      for {key, value} <- parameters,
          into: %{},
          do: {key |> Atom.to_string() |> String.downcase() |> String.to_atom(), value}

    odbc_parameters =
      %{
        servernode: "#{normalized_parameters[:hostname]}:#{normalized_parameters[:port]}",
        Uid: normalized_parameters[:username],
        Pwd: normalized_parameters[:password],
        databasename: normalized_parameters[:database],
        DSN: "SAPHana"
      }
      |> Map.merge(schema_option(default_schema()))
      |> add_optional_parameters(parameters)

    RODBC.connect!(odbc_parameters)
  end

  @impl Driver
  defdelegate disconnect(connection), to: RODBC

  @impl Driver
  def load_tables(connection, table) do
    statement =
      "SELECT column_name, lower(data_type_name) FROM table_columns " <>
        "WHERE table_name = '#{table.db_name}' AND schema_name = '#{default_schema() || "SYS"}' ORDER BY position DESC"

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

  if Mix.env() == :prod do
    # We don't allow env based override in prod
    defp default_schema_from_os_env(), do: nil
  else
    defp default_schema_from_os_env(), do: System.get_env("DEFAULT_SAP_HANA_SCHEMA")
  end

  defp default_schema_from_app_config() do
    with {:ok, saphana_settings} <- Application.fetch_env(:cloak, :sap_hana),
         {:ok, default_schema} <- Keyword.fetch(saphana_settings, :default_schema) do
      default_schema
    else
      _ -> nil
    end
  end

  defp non_empty_schema(nil), do: nil
  defp non_empty_schema(""), do: nil
  defp non_empty_schema(schema) when is_binary(schema), do: schema

  defp schema_option(nil), do: %{}
  defp schema_option(schema), do: %{cs: ~s/"#{schema}"/}

  # Allows for adding additional ODBC connection parameters in the case where
  # a SQL Server installation requires additional parameters.
  defp add_optional_parameters(default_params, %{odbc_parameters: additonal_parameters}),
    do: Map.merge(default_params, additonal_parameters)

  defp add_optional_parameters(default_params, _), do: default_params

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
