defmodule Cloak.DataSource.SAPHana do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP HANA.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.ODBC

  use Cloak.DataSource.Driver.SQL

  @doc """
  Returns the SAP HANA schema as configured in app config.

  This is useful in development, to allow different developers to work on different schemas.
  """
  @spec default_schema() :: nil | String.t()
  def default_schema(),
    do:
      non_empty_schema(default_schema_from_os_env()) ||
        non_empty_schema(default_schema_from_app_config())

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

    ODBC.connect!(odbc_parameters)
  end

  @impl Driver
  defdelegate disconnect(connection), to: ODBC

  @impl Driver
  def load_tables(connection, table),
    do: ODBC.load_tables(connection, update_in(table.db_name, &~s/"#{&1}"/))

  @impl Driver
  defdelegate select(connection, sql_query, result_processor), to: ODBC

  @impl Driver
  defdelegate driver_info(connection), to: ODBC

  @impl Driver
  defdelegate supports_connection_sharing?(), to: ODBC

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
end
