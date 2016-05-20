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
    for [user_id, property] <- rows, do: :aggregator.add_property(property, user_id, aggregator)
    aggregated_buckets = :aggregator.buckets(aggregator)
    anonymized_buckets = :anonymizer.anonymize(aggregated_buckets, lcf_users)
    :aggregator.delete(aggregator)
    :lcf_users.delete(lcf_users)

    {:buckets, columns, anonymized_buckets}
  end
end
