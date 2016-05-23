defmodule Cloak.Task do
  @moduledoc "Cloak task runner."
  require Logger

  defstruct [:id, :query]

  @type t :: %__MODULE__{
    id: String.t,
    query: String.t
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Runs the task and reports the result to the given destination."
  @spec run(t, :result_sender.result_destination) :: :ok
  def run(task, result_target \\ :air_socket) do
    :cloak_metrics.count("task.started")
    start_time = :erlang.monotonic_time(:milli_seconds)

    result = execute_task(task)
    :cloak_metrics.count("task.successful")
    :result_sender.send_result(task.id, result_target, result)

    task_execution_time = :erlang.monotonic_time(:milli_seconds) - start_time
    :cloak_metrics.histogram("task.total", task_execution_time)
    Logger.info("Task #{task.id} executed in #{task_execution_time} ms")

    :ok
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp execute_task(task) do
    {_count, [_user_id | columns], rows} = Cloak.DataSource.query!(:local, task.query)

    reportable_buckets = group_by_user(rows)
    |> pre_process
    |> anonymize
    |> post_process(columns)

    {:buckets, columns, reportable_buckets}
  end

  defp pre_process(rows_by_user) do
    Cloak.Processor.AccumulateCount.pre_process(rows_by_user)
  end

  defp anonymize(properties) do
    lcf_users = :lcf_users.new()
    aggregator = :aggregator.new(lcf_users)

    for [user_id, property] <- properties, do: :aggregator.add_property(property, user_id, aggregator)
    aggregated_buckets = :aggregator.buckets(aggregator)
    anonymized_buckets = :anonymizer.anonymize(aggregated_buckets, lcf_users)

    :aggregator.delete(aggregator)
    :lcf_users.delete(lcf_users)

    anonymized_buckets
  end

  defp post_process(buckets, columns) do
    {lcf_buckets, non_lcf_buckets} = Cloak.Processor.LowCountFilter.process_lcf(buckets, columns)
    post_processed_buckets = Cloak.Processor.AccumulateCount.post_process(non_lcf_buckets)
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
