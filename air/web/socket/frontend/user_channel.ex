defmodule Air.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to queries.
  For the time being no incoming messages are supported,
  but we do support two outgoing type of messages:

  - __result__: reports new results as queries finish executing
  """
  use Air.Web, :channel
  require Logger
  alias Air.Query


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Broadcasts the results of a query execution to all listening clients.
  """
  @spec broadcast_result(Query.t) :: :ok
  def broadcast_result(query) do
    payload = query |> Query.for_display() |> Map.put(:user_id, query.user_id)
    Air.Endpoint.broadcast_from!(self(), "session:#{query.session_id}", "result", payload)
    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @doc false
  @dialyzer {:nowarn_function, join: 3} # Phoenix bug, fixed in master
  def join("session:" <> session_id, _, socket) do
    case Ecto.UUID.cast(session_id) do
      {:ok, _} -> {:ok, socket}
      _ -> {:error, %{success: false, description: "Channel not found"}}
    end
  end

  intercept ["result"]

  @doc false
  def handle_out("result", msg, socket) do
    if socket.assigns.user.id == msg.user_id do
      push(socket, "result", Map.delete(msg, :user_id))
    else
      # Drop message
    end

    {:noreply, socket}
  end
end
