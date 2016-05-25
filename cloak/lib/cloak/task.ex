defmodule Cloak.Task do
  @moduledoc """
  Module for starting tasks.

  This module implements the main server process which starts the task concurrently,
  waits for it to respond, and sends the result back. If the task terminates abnormally,
  the server will send an error result back.
  """
  use GenServer
  require Logger

  alias Cloak.Task.Runner

  defstruct [:id, :query]

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
      runner: Task.async(fn() -> Runner.run(task) end)
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
end
