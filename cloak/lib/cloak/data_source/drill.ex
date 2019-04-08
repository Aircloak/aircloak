defmodule Cloak.DataSource.Drill do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Apache Drill using the Rust ODBC port driver.
  For more information, see `DataSource`.
  """

  use Cloak.DataSource.Driver.RodbcSql
  alias Cloak.DataSource.{RODBC, SqlBuilder}
  alias Cloak.Sql.{Expression, Query}

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @doc "Converts the connection parameters from the config format into the ODBC format."
  @spec conn_params(Map.t()) :: Map.t()
  def conn_params(normalized_parameters) do
    params = %{
      HOST: normalized_parameters[:hostname],
      Schema: normalized_parameters[:database],
      DSN: "MapRDrill"
    }

    port = normalized_parameters[:port]
    params = if port != nil and port != 0, do: Map.put_new(params, :PORT, port), else: params

    if normalized_parameters[:username] != nil do
      Map.merge(params, %{
        UID: normalized_parameters[:username],
        PWD: normalized_parameters[:password],
        AuthenticationType: "Plain"
      })
    else
      params
    end
  end

  @impl Driver
  def connect(parameters), do: RODBC.connect(parameters, &conn_params/1)

  @impl Driver
  def load_tables(connection, table) do
    # In order to workaround some Drill bugs, we need to use `LIMIT 1` when detecting columns.
    # Both `WHERE 1=0` and `LIMIT 0` crash, in some scenarios, on version 1.14.
    RODBC.load_tables(connection, table, &"SELECT * FROM #{SqlBuilder.quote_table_name(&1, ?`)} LIMIT 1")
  end

  @impl Driver
  def supports_query?(query),
    do: not has_cross_joins?(query) and not has_grouping_sets?(query) and not has_trunc_quarter?(query)

  @impl Driver
  def select(connection, query, result_processor) do
    RODBC.select(connection, query, custom_mappers(), result_processor)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp has_cross_joins?(query),
    do: query |> get_in([Query.Lenses.joins()]) |> Enum.any?(&(&1.type == :cross_join))

  defp has_grouping_sets?(query), do: length(query.grouping_sets) > 1 and query.type != :anonymized

  defp has_trunc_quarter?(query) do
    query
    |> get_in([Query.Lenses.all_queries() |> Query.Lenses.query_expressions()])
    |> Enum.any?(&match?(%Expression{function: "date_trunc", function_args: [%Expression{value: "quarter"} | _]}, &1))
  end

  defp custom_mappers() do
    %{
      :interval => &interval_mapper/1
    }
  end

  defp interval_mapper(nil), do: nil

  defp interval_mapper(interval) do
    with [days, time] <- String.split(interval, " "),
         {days, ""} <- Integer.parse(days),
         {:ok, time} <- Time.from_iso8601(time) do
      Timex.Duration.add(Timex.Duration.from_days(days), Timex.Duration.from_time(time))
    else
      _ -> nil
    end
  end
end
