defmodule Air.Socket.Frontend.TaskChannel do
  @moduledoc """
  Channel used for communicating events related to tasks.
  For the time being no incoming messages are supported,
  but we do support two outgoing type of messages:

  - __progress__: reports on the progress of task execution
  - __result__: reports new results as tasks finish executing
  """
  use Air.Web, :channel
  require Logger
  alias Air.Task


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Broadcasts a progress message to all clients subscribing to a task.
  This allows the interface to keep progress meters up to date.

  The `progress` parameter should be an integer between 0 and 100
  indicating to what percent the task execution is complete.
  """
  @spec broadcast_progress(Task.t, integer) :: :ok
  def broadcast_progress(task, progress) do
    payload = %{
      progress: progress
    }
    Air.Endpoint.broadcast_from!(self(), "task:#{task.id}", "progress", payload)
    :ok
  end

  @doc """
  Broadcasts the results of a task execution to all listening clients.
  """
  @spec broadcast_result(Task.result) :: :ok
  def broadcast_result(result) do
    Air.Endpoint.broadcast_from!(self(), "task:#{result["task_id"]}", "result", result)
    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  def join("task:" <> task_id, _, socket) do
    user_id = socket.assigns.user.id
    case Repo.get(Task, task_id) do
      %Task{user_id: id} when id == user_id -> {:ok, socket}
      _ -> {:error, %{success: false, description: "Task not found"}}
    end
  end
end
