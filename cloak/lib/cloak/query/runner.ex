defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."

  alias Cloak.{Aggregator, LCFData, DataSource, Processor}
  alias Cloak.Query.Result

  use Cloak.Type

  @doc "Runs the query and returns the result."
  @spec run(Cloak.Query.t) :: {:ok, {:buckets, [DataSource.column], [Bucket.t]}} | {:error, any}
  def run(query) do
    with {:ok, sql_query} <- Cloak.SqlQuery.parse(query.statement),
         :ok <- validate(sql_query, query.data_source) do
      execute_sql_query(sql_query, query.data_source)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp validate(sql_query, data_source) do
    with :ok <- validate_from(sql_query, data_source),
         :ok <- validate_columns(sql_query, data_source),
         :ok <- validate_aggregation(sql_query),
         :ok <- validate_order_by(sql_query), do: :ok
  end

  defp validate_from(%{from: table_identifier}, data_source) do
    case Enum.find_index(DataSource.tables(data_source), &(Atom.to_string(&1) === table_identifier)) do
      nil -> {:error, ~s/Table "#{table_identifier}" doesn't exist./}
      _ -> :ok
    end
  end
  defp validate_from(%{}, _data_source), do: :ok

  defp validate_aggregation(%{command: :select} = query) do
    case invalid_not_aggregated_columns(query) do
      [] -> :ok
      columns ->
        {
          :error,
          "Columns #{columns |> Enum.map(&Result.column_title/1) |> Enum.join} need to appear in the " <>
            "`group by` clause or be used in an aggregate function."
        }
    end
  end
  defp validate_aggregation(_), do: :ok

  defp invalid_not_aggregated_columns(%{command: :select, group_by: [_|_]} = query) do
    Enum.reject(query.columns, &(aggregate_function?(&1) || Enum.member?(query.group_by, &1)))
  end
  defp invalid_not_aggregated_columns(%{command: :select} = query) do
    case Enum.partition(query.columns, &aggregate_function?/1) do
      {[_|_] = _aggregates, [_|_] = non_aggregates} -> non_aggregates
      _ -> []
    end
  end

  defp aggregate_function?({:count, _}), do: true
  defp aggregate_function?(_), do: false

  defp validate_columns(%{command: :select, from: table_identifier} = query, data_source) do
    table_id = String.to_existing_atom(table_identifier)
    invalid_columns = Enum.reject(all_columns(query), &valid_column?(&1, DataSource.columns(data_source, table_id)))
    case invalid_columns do
      [] -> :ok
      [invalid_column | _rest] -> {:error, ~s/Column "#{invalid_column}" doesn't exist./}
    end
  end
  defp validate_columns(%{}, _data_source), do: :ok

  defp valid_column?({:count, :star}, _), do: true
  defp valid_column?(name, columns) do
    columns
    |> Enum.any?(fn {column, _} -> name == column end)
  end

  defp all_columns(%{columns: selected_columns} = query) do
    where_columns = Map.get(query, :where, [])
    |> Enum.map(&where_clause_to_identifier/1)

    group_by_columns = Map.get(query, :group_by, [])

    selected_columns ++ where_columns ++ group_by_columns
  end

  defp where_clause_to_identifier({:comparison, identifier, _, _}), do: identifier
  defp where_clause_to_identifier({:in, identifier, _}), do: identifier
  defp where_clause_to_identifier({:like, identifier, _}), do: identifier

  defp validate_order_by(%{columns: columns, order_by: order_by_spec}) do
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) -> Enum.member?(columns, column) end)
    case invalid_fields do
      [] -> :ok
      [{invalid_field, _direction} | _rest] ->
        {:error, ~s/Non-selected field specified in 'order by' clause: #{inspect invalid_field}./}
    end
  end
  defp validate_order_by(%{}), do: :ok

  defp execute_sql_query(%{command: :show, show: :tables}, data_source) do
    columns = ["name"]
    rows = Enum.map(DataSource.tables(data_source), &[&1])

    {:ok, {:buckets, columns, rows}}
  end
  defp execute_sql_query(%{command: :show, show: :columns, from: table_identifier}, data_source) do
    table_id = String.to_existing_atom(table_identifier)
    columns = ["name", "type"]
    rows = Enum.map(DataSource.columns(data_source, table_id), &Tuple.to_list/1)

    {:ok, {:buckets, columns, rows}}
  end
  defp execute_sql_query(%{command: :select} = select_query, data_source) do
    with {:ok, {_count, [_user_id | columns], rows}} <- DataSource.select(data_source, select_query) do
      lcf_data = LCFData.new()

      reportable_buckets = try do
        group_by_user(rows)
        |> Processor.AccumulateCount.pre_process()
        |> anonymize(lcf_data)
        |> Processor.AccumulateCount.post_process()
        |> add_lcf_buckets(lcf_data, length(columns))
        |> Result.apply_aggregation(select_query)
        |> Result.apply_order(select_query)
        |> Result.expand()
      after
        LCFData.delete(lcf_data)
      end

      {:ok, {:buckets, Result.column_titles(select_query), reportable_buckets}}
    end
  end

  defp anonymize(properties, lcf_data) do
    aggregator = Aggregator.new()

    try do
      for {user_id, property} <- properties, do: Aggregator.add_property(aggregator, user_id, property)
      Aggregator.gather_buckets(aggregator, lcf_data) |> :anonymizer.anonymize(lcf_data)
    after
      Aggregator.delete(aggregator)
    end
  end

  defp add_lcf_buckets(buckets, lcf_data, columns_count) do
    lcf_property = List.duplicate(:*, columns_count)
    low_count_filter_data = LCFData.filtered_property_counts(lcf_data)
    |> Enum.map(fn({user, count}) -> {user, List.duplicate(lcf_property, count)} end)

    lcf_buckets = Processor.AccumulateCount.pre_process(low_count_filter_data)
    |> anonymize(:undefined)
    |> Processor.AccumulateCount.post_process

    buckets ++ lcf_buckets
  end

  defp group_by_user(rows) do
    rows
    |> Enum.reduce(%{}, fn([user | property], accumulator) ->
        Map.update(accumulator, user, [property], fn(existing_properties) -> [property | existing_properties] end)
      end)
    |> Enum.to_list
  end
end
