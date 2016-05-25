defmodule Cloak.Query.Runner do
  @moduledoc "Cloak query runner."
  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  alias Cloak.LCFData

  @type bucket :: record(:bucket, property: [any], noisy_count: pos_integer)

  @doc "Runs the query and returns the result."
  @spec run(Cloak.Query.t) ::
    {:buckets, [Cloak.DataSource.column], [Cloak.DataSource.row]} |
    {:error, any}
  def run(query) do
    with {:ok, query_result} <- Cloak.DataSource.query(:local, query.aql) do
      {_count, [_user_id | columns], rows} = query_result
      lcf_data = LCFData.new()

      reportable_buckets = group_by_user(rows)
      |> Cloak.Processor.AccumulateCount.pre_process()
      |> anonymize(lcf_data)
      |> post_process(lcf_data, length(columns))

      LCFData.delete(lcf_data)

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
    post_processed_buckets = Cloak.Processor.AccumulateCount.post_process(buckets)

    # We also want to account for the number of low count filtered properties
    lcf_property = List.duplicate("*", columns_count)
    low_count_filter_data = LCFData.filtered_property_counts(lcf_data)
    |> Enum.map(fn({user, count}) -> {user, List.duplicate(lcf_property, count)} end)

    lcf_buckets = Cloak.Processor.AccumulateCount.pre_process(low_count_filter_data)
    |> anonymize(:undefined)
    |> Cloak.Processor.AccumulateCount.post_process

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
