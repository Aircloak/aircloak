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
    reportable_buckets = group_by_user(rows)
    |> pre_process()
    |> anonymize(columns)
    |> post_process()

    {:buckets, columns, reportable_buckets}
  end

  defp pre_process(rows_by_user) do
    Cloak.Processor.AccumulateCount.pre_process(rows_by_user)
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
    {lcf_buckets, other_buckets} = Enum.partition(buckets, &(bucket(&1, :property) == ["aircloak_lcf_tail"]))

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
