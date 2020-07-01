defmodule Cloak.DataSource.ClouderaImpala do
  @moduledoc """
  Implements the DataSource.Driver behaviour for Cloudera Data Platform (CDP) Impala.
  This implementation targets Impala 2.10 of CDH 5.13.x Enterprise Edition.
  For more information, see `DataSource`.
  """

  alias Cloak.DataSource.RODBC
  use Cloak.DataSource.Driver.RodbcSql
  @default_port 21050

  # -------------------------------------------------------------------
  # DataSource.Driver callbacks
  # -------------------------------------------------------------------

  @impl Driver
  def connect(parameters), do: RODBC.connect(parameters, &conn_params/1)

  @impl Driver
  def select(connection, query, result_processor) do
    statement = SqlBuilder.build(query)
    RODBC.select(connection, statement, query.db_columns, custom_mappers(), result_processor)
  end

  @impl Driver
  def supports_query?(query),
    do: query.type == :anonymized or (length(query.grouping_sets) <= 1 and distinct_aggregators_count(query) <= 1)

  @impl Driver
  def load_tables(connection, table),
    do:
      connection
      |> RODBC.load_tables(table)
      |> Enum.map(&update_column_names/1)
      |> Enum.map(&load_comments(connection, &1))

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_column_names(table),
    do: %{table | columns: Enum.map(table.columns, &update_column_name(table.db_name, &1))}

  defp update_column_name(table_name, column),
    do: %{column | name: String.replace_prefix(column.name, "#{table_name}.", "")}

  defp conn_params(normalized_parameters) do
    %{
      DSN: "Cloudera Impala",
      HOST: normalized_parameters[:hostname],
      PORT: Map.get(normalized_parameters, :port, @default_port),
      Schema: normalized_parameters.database || "default",
      UID: normalized_parameters[:username],
      PWD: normalized_parameters[:password]
    }
  end

  defp load_comments(connection, table) do
    table_comments =
      connection
      |> select("DESCRIBE EXTENDED #{table.db_name}")
      |> case do
        {:ok, results} ->
          results
          |> Enum.find_value(nil, fn
            ["", "comment" <> _, comment] -> comment
            _ -> nil
          end)

        {:error, _} ->
          %{}
      end

    column_comments =
      connection
      |> select("DESCRIBE #{table.db_name}")
      |> case do
        {:ok, results} ->
          results
          |> Enum.reject(fn [_name, _type, comment] -> is_nil(comment) end)
          |> Enum.map(fn [name, _type, comment] -> {name, comment} end)
          |> Enum.into(%{})

        {:error, _} ->
          %{}
      end

    comments =
      %{table: table_comments, columns: column_comments}
      |> Aircloak.deep_merge(Map.get(table, :comments, %{}))

    Map.put(table, :comments, comments)
  end

  defp custom_mappers() do
    %{
      :date => nil_mapper(&date_mapper/1),
      :time => nil_mapper(&time_mapper/1),
      :datetime => nil_mapper(&datetime_mapper/1)
    }
  end

  defp nil_mapper(mapper) do
    fn
      nil -> nil
      other -> mapper.(other)
    end
  end

  defp datetime_mapper(string) do
    case Cloak.Time.parse_datetime(string) do
      {:ok, datetime} -> datetime
      {:error, _reason} -> nil
    end
  end

  defp date_mapper(string) do
    case Cloak.Time.parse_datetime(string) do
      {:ok, datetime} -> NaiveDateTime.to_date(datetime)
      {:error, _reason} -> nil
    end
  end

  defp time_mapper(string) do
    case Cloak.Time.parse_datetime(string) do
      {:ok, datetime} -> NaiveDateTime.to_time(datetime)
      {:error, _reason} -> nil
    end
  end

  defp distinct_aggregators_count(query),
    do: query.aggregators |> Enum.filter(&match?(%Cloak.Sql.Expression{args: [{:distinct, _}]}, &1)) |> Enum.count()
end
