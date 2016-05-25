defmodule Cloak.Task do
  @moduledoc "Cloak task runner."
  use GenServer
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

  @sup_name Module.concat(__MODULE__, Supervisor)


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Returns the supervisor specification for the task runner supervisor."
  @spec supervisor_spec() :: Supervisor.Spec.spec
  def supervisor_spec() do
    import Supervisor.Spec, warn: false

    supervisor(Supervisor, [
      [worker(GenServer, [__MODULE__], restart: :temporary)],
      [id: @sup_name, name: @sup_name, strategy: :simple_one_for_one]
    ])
  end

  @doc """
  Starts the task execution concurrently.

  This function returns as soon as the task runner process is started. The result
  is sent to the required destination. If an error occurs, the result will contain
  error information.
  """
  @spec start(t, :result_sender.result_destination) :: :ok
  def start(task, result_target \\ :air_socket) do
    {:ok, _} = Supervisor.start_child(@sup_name, [{task, result_target}])
    :ok
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init({task, result_target}) do
    Process.flag(:trap_exit, true)
    {:ok, %{
      task_id: task.id,
      result_target: result_target,
      start_time: :erlang.monotonic_time(:milli_seconds),
      # We're starting the runner as a direct child.
      # This GenServer will wait for the runner to return or crash. Such approach allows us to
      # detect a failure no matter how the task fails (even if the runner process is for example killed).
      runner: Task.async(fn() -> run_task(task) end)
    }}
  end

  @doc false
  def handle_info({:EXIT, runner_pid, reason}, %{runner: %Task{pid: runner_pid}} = state) do
    if reason != :normal do
      :result_sender.send_result(state.task_id, state.result_target, {:error, "Cloak error"})
      :cloak_metrics.count("task.error")
    end

    # Note: we're always exiting with a reason normal. If a task crashed, the error will be
    # properly logged, so no need to add more noise.
    {:stop, :normal, state}
  end
  def handle_info(msg, %{runner: runner} = state) do
    with {result, ^runner} <- Task.find([runner], msg) do
      case result do
        {:ok, result} ->
          :result_sender.send_result(state.task_id, state.result_target, result)
          :cloak_metrics.count("task.success")
          task_execution_time = :erlang.monotonic_time(:milli_seconds) - state.start_time
          :cloak_metrics.histogram("task.total", task_execution_time)
          Logger.info("Task #{state.task_id} executed in #{task_execution_time} ms")
        {:error, reason} ->
          :result_sender.send_result(state.task_id, state.result_target, {:error, reason})
          :cloak_metrics.count("task.error")
      end
    end
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp run_task(task) do
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
