defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."
  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  alias Cloak.{LCFData, DataSource, Processor}

  @type bucket :: record(:bucket, property: [any], noisy_count: pos_integer)

  @doc "Runs the query and returns the result."
  @spec run(Cloak.Query.t) :: {:ok, {:buckets, [DataSource.column], [bucket]}} | {:error, any}
  def run(query) do
    with {:ok, sql_query} <- Cloak.SqlQuery.parse(query.statement), :ok <- validate(sql_query) do
       execute_sql_query(sql_query)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp validate(sql_query) do
    with :ok <- validate_from(sql_query), :ok <- validate_columns(sql_query), do: :ok
  end

  defp validate_from(%{from: table_identifier}) do
    case Enum.find_index(DataSource.tables(:local), &(Atom.to_string(&1) === table_identifier)) do
      nil -> {:error, ~s/Table "#{table_identifier}" doesn't exist./}
      _ -> :ok
    end
  end
  defp validate_from(%{}), do: :ok

  defp validate_columns(%{statement: :select, columns: selected_columns, from: table_identifier}) do
    table_id = String.to_existing_atom(table_identifier)
    invalid_columns = Enum.reject(selected_columns, &valid_column?(&1, DataSource.columns(:local, table_id)))
    case invalid_columns do
      [] -> :ok
      [invalid_column | _rest] -> {:error, ~s/Column "#{invalid_column}" doesn't exist./}
    end
  end
  defp validate_columns(%{}), do: :ok

  defp valid_column?({:count, :star}, _), do: true
  defp valid_column?(name, columns), do: Enum.any?(columns, fn {column, _} -> name == column end)

  defp execute_sql_query(%{statement: :show, show: :tables}) do
    tables = DataSource.tables(:local)
    buckets = for table <- tables, do: bucket(property: [table], noisy_count: 1)
    {:ok, {:buckets, ["name"], buckets}}
  end
  defp execute_sql_query(%{statement: :show, show: :columns, from: table_identifier}) do
    table_id = String.to_existing_atom(table_identifier)
    columns = DataSource.columns(:local, table_id)
    buckets = for {name, type} <- columns, do: bucket(property: [name, type], noisy_count: 1)
    {:ok, {:buckets, ["name", "type"], buckets}}
  end
  defp execute_sql_query(%{statement: :select} = select_query) do
    with {:ok, {_count, [_user_id | columns], rows}} <- DataSource.select(:local, select_query) do
      lcf_data = LCFData.new()

      reportable_buckets = try do
        group_by_user(rows)
        |> Processor.AccumulateCount.pre_process()
        |> anonymize(lcf_data)
        |> post_process(lcf_data, length(columns))
      after
        LCFData.delete(lcf_data)
      end

      {:ok, {:buckets, columns, reportable_buckets}}
    end
  end

  defp anonymize(properties, lcf_data) do
    aggregator = :aggregator.new(lcf_data)

    for {user_id, property} <- properties, do: :aggregator.add_property(property, user_id, aggregator)
    aggregated_buckets = :aggregator.buckets(aggregator)
    anonymized_buckets = :anonymizer.anonymize(aggregated_buckets, lcf_data)

    :aggregator.delete(aggregator)

    anonymized_buckets
  end

  defp post_process(buckets, lcf_data, columns_count) do
    post_processed_buckets = Processor.AccumulateCount.post_process(buckets)

    # We also want to account for the number of low count filtered properties
    lcf_property = List.duplicate("*", columns_count)
    low_count_filter_data = LCFData.filtered_property_counts(lcf_data)
    |> Enum.map(fn({user, count}) -> {user, List.duplicate(lcf_property, count)} end)

    lcf_buckets = Processor.AccumulateCount.pre_process(low_count_filter_data)
    |> anonymize(:undefined)
    |> Processor.AccumulateCount.post_process

    post_processed_buckets ++ lcf_buckets
  end

  defp group_by_user(rows) do
    rows
    |> Enum.reduce(%{}, fn([user | property], accumulator) ->
        Map.update(accumulator, user, [property], fn(existing_properties) -> [property | existing_properties] end)
      end)
    |> Enum.to_list
  end
end
