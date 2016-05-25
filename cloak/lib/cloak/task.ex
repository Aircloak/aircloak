defmodule Cloak.Task do
  @moduledoc "Cloak task runner."
  require Logger
  require Record

  import Record, only: [defrecord: 2, extract: 2]
  defrecord :bucket, extract(:bucket, from_lib: "cloak/include/cloak.hrl")

  defstruct [:id, :query]

  @type bucket :: record(:bucket, property: [any], noisy_count: pos_integer)
  @type t :: %__MODULE__{
    id: String.t,
    query: String.t
  }

  alias Cloak.LCFData


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs the task and reports the result to the given destination."
  @spec run(t, :result_sender.result_destination) :: :ok | {:error, any}
  def run(task, result_target \\ :air_socket) do
    :cloak_metrics.count("task.started")
    start_time = :erlang.monotonic_time(:milli_seconds)

    response =
      with {:ok, result} <- execute_task(task) do
        :cloak_metrics.count("task.successful")
        :result_sender.send_result(task.id, result_target, result)
        :ok
      end

    task_execution_time = :erlang.monotonic_time(:milli_seconds) - start_time
    :cloak_metrics.histogram("task.total", task_execution_time)
    :cloak_metrics.count("task.finished")
    Logger.info("Task #{task.id} executed in #{task_execution_time} ms")

    response
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp execute_task(task) do
    with {:ok, query_result} <- Cloak.DataSource.query(:local, task.query), do:
      {:ok, process_query(query_result)}
  end

  defp process_query({_count, [_user_id | columns], rows}) do
    lcf_data = LCFData.new()

    reportable_buckets = group_by_user(rows)
    |> pre_process()
    |> anonymize(lcf_data)
    |> post_process(lcf_data, length(columns))

    LCFData.delete(lcf_data)

    {:buckets, columns, reportable_buckets}
  end

  defp pre_process(rows_by_user) do
    Cloak.Processor.AccumulateCount.pre_process(rows_by_user)
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
