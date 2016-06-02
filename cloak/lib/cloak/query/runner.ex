defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."

  alias Cloak.{Aggregator, LCFData, DataSource, Processor}
  alias Cloak.Query.Result

  use Cloak.Type

  @doc "Runs the query and returns the result."
  @spec run(Cloak.Query.t) :: {:ok, {:buckets, [DataSource.column], [Bucket.t]}} | {:error, any}
  def run(query) do
    with {:ok, sql_query} <- Cloak.SqlQuery.parse(query.statement), :ok <- validate(sql_query) do
      execute_sql_query(sql_query)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp validate(sql_query) do
    with :ok <- validate_from(sql_query),
         :ok <- validate_columns(sql_query),
         :ok <- validate_aggregation(sql_query),
         :ok <- validate_order_by(sql_query), do: :ok
  end

  defp validate_from(%{from: table_identifier}) do
    case Enum.find_index(DataSource.tables(:local), &(Atom.to_string(&1) === table_identifier)) do
      nil -> {:error, ~s/Table "#{table_identifier}" doesn't exist./}
      _ -> :ok
    end
  end
  defp validate_from(%{}), do: :ok

  defp validate_aggregation(%{command: :select, columns: columns} = query) do
    if invalid = Enum.find(columns, fn column -> !valid_aggregation?(column, query) end) do
      {
        :error,
        "Column #{Result.column_title(invalid)} needs to appear in the `group by` clause or be used in an " <>
          "aggregate function"
      }
    else
      :ok
    end
  end
  defp validate_aggregation(_), do: :ok

  defp valid_aggregation?(column, query) do
    aggregate_column?(column, query) == aggregate_query?(query)
  end

  defp aggregate_query?(%{group_by: [_ | _]}), do: true
  defp aggregate_query?(%{columns: columns} = query) do
    Enum.any?(columns, &aggregate_column?(&1, query))
  end

  defp aggregate_column?(column, query) do
    aggregate_function?(column) || grouped_column?(column, query)
  end

  defp aggregate_function?({:count, _}), do: true
  defp aggregate_function?(_), do: false

  defp grouped_column?(column, %{group_by: group_bys}), do: Enum.member?(group_bys, column)
  defp grouped_column?(_, _), do: false

  defp validate_columns(%{command: :select, columns: selected_columns, from: table_identifier} = query) do
    table_id = String.to_existing_atom(table_identifier)
    selected_columns = selected_columns ++ Map.get(query, :group_by, [])
    invalid_columns = Enum.reject(selected_columns, &valid_column?(&1, DataSource.columns(:local, table_id)))
    case invalid_columns do
      [] -> :ok
      [invalid_column | _rest] -> {:error, ~s/Column "#{invalid_column}" doesn't exist./}
    end
  end
  defp validate_columns(%{}), do: :ok

  defp valid_column?({:count, :star}, _), do: true
  defp valid_column?(name, columns) do
    columns
    |> Enum.any?(fn {column, _} -> name == column end)
  end

  defp validate_order_by(%{columns: columns, order_by: order_by_spec}) do
    invalid_fields = Enum.reject(order_by_spec, fn ({column, _direction}) -> Enum.member?(columns, column) end)
    case invalid_fields do
      [] -> :ok
      [{invalid_field, _direction} | _rest] ->
        {:error, ~s/Non-selected field specified in 'order by' clause: #{inspect invalid_field}./}
    end
  end
  defp validate_order_by(%{}), do: :ok

  defp execute_sql_query(%{command: :show, show: :tables}) do
    tables = DataSource.tables(:local)
    buckets = for table <- tables, do: bucket(property: [table], noisy_count: 1)
    {:ok, {:buckets, ["name"], buckets}}
  end
  defp execute_sql_query(%{command: :show, show: :columns, from: table_identifier}) do
    table_id = String.to_existing_atom(table_identifier)
    columns = DataSource.columns(:local, table_id)
    buckets = for {name, type} <- columns, do: bucket(property: [name, type], noisy_count: 1)
    {:ok, {:buckets, ["name", "type"], buckets}}
  end
  defp execute_sql_query(%{command: :select} = select_query) do
    with {:ok, {_count, [_user_id | columns], rows}} <- DataSource.select(:local, select_query) do
      lcf_data = LCFData.new()

      reportable_buckets = try do
        group_by_user(rows)
        |> Processor.AccumulateCount.pre_process()
        |> anonymize(lcf_data)
        |> Processor.AccumulateCount.post_process()
        |> order_buckets(select_query)
        |> add_lcf_buckets(lcf_data, length(columns))
        |> Result.expand(select_query)
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
    lcf_property = List.duplicate("*", columns_count)
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

  defp order_buckets(buckets, %{columns: columns, order_by: order_by_spec}) do
    order_list = for {column, direction} <- order_by_spec do
      index = columns |> Enum.find_index(&(&1 == column))
      {index, direction}
    end
    buckets |> Enum.sort(fn (bucket(property: row1), bucket(property: row2)) ->
      compare_rows(row1, row2, order_list)
    end)
  end
  defp order_buckets(buckets, %{}), do: buckets

  defp compare_rows(row1, row2, []), do: row1 < row2
  defp compare_rows(row1, row2, [{index, direction} | remaining_order]) do
    field1 = row1 |> Enum.at(index)
    field2 = row2 |> Enum.at(index)
    case field1 === field2 do
      :true -> compare_rows(row1, row2, remaining_order)
      :false -> compare_fields(field1, field2, direction)
    end
  end

  defp compare_fields(field1, field2, nil), do: compare_fields(field1, field2, :asc)
  defp compare_fields(field1, field2, :asc), do: field1 < field2
  defp compare_fields(field1, field2, :desc), do: field1 > field2
end
