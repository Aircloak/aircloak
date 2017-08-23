defmodule Cloak.DataSource.SAPHana do
  @moduledoc """
  Implements the DataSource.Driver behaviour for SAP HANA.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.ODBC


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @doc false
  def dialect(_parameters), do: :saphana

  @doc false
  def connect!(parameters) do
    normalized_parameters = for {key, value} <- parameters, into: %{}, do:
      {key |> Atom.to_string() |> String.downcase() |> String.to_atom(), value}

    odbc_parameters = %{
      "servernode": "#{normalized_parameters[:hostname]}:#{normalized_parameters[:port]}",
      "Uid": normalized_parameters[:username],
      "Pwd": normalized_parameters[:password],
      "databasename": normalized_parameters[:database],
    }
    |> Map.merge(driver_option())
    |> add_optional_parameters(parameters)
    ODBC.connect!(odbc_parameters)
  end

  defdelegate disconnect(connection), to: ODBC

  defdelegate load_tables(connection, table), to: ODBC

  defdelegate select(connection, sql_query, result_processor), to: ODBC

  defdelegate supports_query?(query), to: ODBC


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp driver_option() do
    if System.get_env("TRAVIS") == "true" do
      %{"DSN": "SAPHANA"}
    else
      %{
        "dialect": :saphana,
        "driver": "#{Application.app_dir(:cloak, "priv/odbc/drivers")}/libodbc-sap-hana-v2.so"
      }
    end
  end

  # Allows for adding additional ODBC connection parameters in the case where
  # a SQL Server installation requires additional parameters.
  defp add_optional_parameters(default_params, %{odbc_parameters: additonal_parameters}), do:
    Map.merge(default_params, additonal_parameters)
  defp add_optional_parameters(default_params, _), do: default_params
end
