defmodule CentralWeb.Socket.Frontend.UserChannel do
  @moduledoc """
  Channel used for communicating events related to queries.
  For the time being no incoming messages are supported,
  but we do support two outgoing type of messages:

  - __result__: reports new results as queries finish executing
  """
  use Central.Web, :channel
  require Logger

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
