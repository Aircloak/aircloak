defmodule Cloak.Task.Runner do
  @moduledoc "Cloak task runner."
  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  @type bucket :: record(:bucket, property: [any], noisy_count: pos_integer)

  @doc "Runs the task and returns the result."
  @spec run(Cloak.Task.t) ::
    {:buckets, [Cloak.DataSource.column], [Cloak.DataSource.row]} |
    {:error, any}
  def run(task) do
    with {:ok, query_result} <- Cloak.DataSource.query(:local, task.query) do
      {_count, [_user_id | columns], rows} = query_result

      reportable_buckets = group_by_user(rows)
      |> Cloak.Processor.AccumulateCount.pre_process()
      |> anonymize(columns)
      |> post_process()

      {:ok, {:buckets, columns, reportable_buckets}}
    end
  end

  defp anonymize(properties, columns) do
    lcf_users = :lcf_users.new()
    aggregator = :aggregator.new(lcf_users)

    for {user_id, property} <- properties, do: :aggregator.add_property(property, user_id, aggregator)
    aggregated_buckets = :aggregator.buckets(aggregator)
    anonymized_buckets = :anonymizer.anonymize(aggregated_buckets, lcf_users, length(columns))

    :aggregator.delete(aggregator)
    :lcf_users.delete(lcf_users)

    anonymized_buckets
  end

  defp post_process(buckets) do
    {lcf_buckets, other_buckets} = Enum.partition(buckets,
      &(is_list(bucket(&1, :property)) && hd(bucket(&1, :property)) == "aircloak_lcf_tail"))

    post_processed_buckets = Cloak.Processor.AccumulateCount.post_process(other_buckets)

    lcf_buckets ++ post_processed_buckets
  end

  defp group_by_user(rows) do
    rows
    |> Enum.reduce(%{}, fn([user | property], accumulator) ->
        Map.update(accumulator, user, [property], fn(existing_properties) -> [property | existing_properties] end)
      end)
    |> Enum.to_list
  end
end
