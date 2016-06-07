defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."

  alias Cloak.{Aggregator, LCFData, DataSource}
  alias Cloak.Processor.{AccumulateCount, NegativeCondition}
  alias Cloak.Query.Result

  use Cloak.Type

  @doc "Runs the query and returns the result."
  @spec run(Cloak.Query.t) :: {:ok, {:buckets, [DataSource.column], [Bucket.t]}} | {:error, any}
  def run(query) do
    with {:ok, sql_query} <- Cloak.SqlQuery.make(query.data_source, query.statement) do
      execute_sql_query(sql_query)
    end
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp execute_sql_query(%{command: :show, show: :tables, data_source: data_source}) do
    columns = ["name"]
    rows = Enum.map(DataSource.tables(data_source), &[&1])

    {:ok, {:buckets, columns, rows}}
  end
  defp execute_sql_query(%{command: :show, show: :columns, from: table_identifier, data_source: data_source}) do
    table_id = String.to_existing_atom(table_identifier)
    columns = ["name", "type"]
    rows = Enum.map(DataSource.columns(data_source, table_id), &Tuple.to_list/1)

    {:ok, {:buckets, columns, rows}}
  end
  defp execute_sql_query(%{command: :select, data_source: data_source} = select_query) do
    with {:ok, {_count, [_user_id | columns], rows}} <- DataSource.select(data_source, select_query) do
      lcf_data = LCFData.new()

      reportable_buckets = try do
        rows
        |> NegativeCondition.apply(select_query)
        |> group_by_user()
        |> AccumulateCount.pre_process()
        |> anonymize(lcf_data)
        |> AccumulateCount.post_process()
        |> add_lcf_buckets(lcf_data, length(columns))
        |> Result.apply_aggregation(select_query)
        |> Result.apply_order(select_query)
        |> Result.expand()
      after
        LCFData.delete(lcf_data)
      end

      {:ok, {:buckets, Cloak.SqlQuery.column_titles(select_query), reportable_buckets}}
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

    lcf_buckets = AccumulateCount.pre_process(low_count_filter_data)
    |> anonymize(:undefined)
    |> AccumulateCount.post_process

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
