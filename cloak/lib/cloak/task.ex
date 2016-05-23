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
    lcf_users = :lcf_users.new()
    aggregator = :aggregator.new(lcf_users)
    {_count, [_user_id | columns], rows} = Cloak.DataSource.query!(:local, task.query)
    rows_by_user = group_by_user(rows)

    pre_processed_properties = Cloak.Processor.AccumulateCount.pre_process(rows_by_user)
    for [user_id, property] <- pre_processed_properties, do: :aggregator.add_property(property, user_id, aggregator)
    aggregated_buckets = :aggregator.buckets(aggregator)
    anonymized_buckets = :anonymizer.anonymize(aggregated_buckets, lcf_users)

    {lcf_buckets, non_lcf_buckets} = Cloak.Processor.LowCountFilter.process_lcf(anonymized_buckets, columns)
    post_processed_buckets = Cloak.Processor.AccumulateCount.post_process(non_lcf_buckets)
    all_buckets = lcf_buckets ++ post_processed_buckets

    :aggregator.delete(aggregator)
    :lcf_users.delete(lcf_users)

    {:buckets, columns, all_buckets}
  end

  defp group_by_user(rows) do
    rows
    |> Enum.reduce(%{}, fn([user | property], accumulator) ->
        Map.update(accumulator, user, [property], fn(existing_properties) -> [property | existing_properties] end)
      end)
    |> Enum.to_list
  end
end
