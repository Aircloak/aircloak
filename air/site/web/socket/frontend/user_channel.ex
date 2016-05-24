defmodule Air.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to tasks.
  For the time being no incoming messages are supported,
  but we do support two outgoing type of messages:

  - __result__: reports new results as tasks finish executing
  """
  use Air.Web, :channel
  require Logger
  alias Air.Task


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

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
  def join("user:" <> user_id, _, socket) do
    if socket.assigns.user.id == String.to_integer(user_id) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Channel not found"}}
    end
  end
end
