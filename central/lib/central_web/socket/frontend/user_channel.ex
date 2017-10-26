defmodule Central.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to queries.
  For the time being no incoming messages are supported,
  but we do support two outgoing type of messages:

  - __result__: reports new results as queries finish executing
  """
  use Central.Web, :channel
  require Logger
  alias Central.Query


  # -------------------------------------------------------------------
  # API
  # -------------------------------------------------------------------

  @doc """
  Broadcasts the results of a query execution to all listening clients.
  """
  @spec broadcast_result(Query.t) :: :ok
  def broadcast_result(query) do
    Central.Endpoint.broadcast_from!(self(), "user:#{query.user_id}", "result", Query.for_display(query))
    :ok
  end


  # -------------------------------------------------------------------
  # Phoenix.Channel callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Channel
  def join("user:" <> user_id, _, socket) do
    if socket.assigns.user.id == String.to_integer(user_id) do
      {:ok, socket}
    else
      {:error, %{success: false, description: "Channel not found"}}
    end
  end
end
