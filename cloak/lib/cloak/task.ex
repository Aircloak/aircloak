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
      {:ok, post_process_query(query_result)}
  end

  defp post_process_query({_count, [_user_id | columns], rows}) do
    lcf_users = :lcf_users.new()
    aggregator = :aggregator.new(lcf_users)
    pre_processed_properties = Cloak.Processor.AccumulateCount.pre_process(rows)
    for [user_id, property] <- pre_processed_properties, do: :aggregator.add_property(property, user_id, aggregator)
    aggregated_buckets = :aggregator.buckets(aggregator)
    anonymized_buckets = :anonymizer.anonymize(aggregated_buckets, lcf_users, length(columns))
    post_processed_properties = Cloak.Processor.AccumulateCount.post_process(anonymized_buckets)

    :aggregator.delete(aggregator)
    :lcf_users.delete(lcf_users)

    {:buckets, columns, post_processed_properties}
  end
end
