defmodule Cloak.DataSource.SQLServer do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Microsoft SQL Server.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.ODBC


  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @behaviour Cloak.DataSource.Driver

  @doc false
  def dialect(_parameters), do: Cloak.DataSource.SqlBuilder.SQLServer

  @doc false
  def connect!(parameters) do
    normalized_parameters = for {key, value} <- parameters, into: %{}, do:
      {key |> Atom.to_string() |> String.downcase() |> String.to_atom(), value}

    odbc_parameters = %{
      "DSN": "SQLServer",
      "Server": normalized_parameters[:hostname],
      "Uid": normalized_parameters[:username],
      "Pwd": normalized_parameters[:password],
      "Database": normalized_parameters[:database],
    }
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

  # Allows for adding additional ODBC connection parameters in the case where
  # a SQL Server installation requires additional parameters.
  defp add_optional_parameters(default_params, %{odbc_parameters: additonal_parameters}), do:
    Map.merge(default_params, additonal_parameters)
  defp add_optional_parameters(default_params, _), do: default_params
end
