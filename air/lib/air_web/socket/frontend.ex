defmodule AirWeb.Socket.Frontend do
  @moduledoc """
  Implements websocket interface for web clients using our interface

  The client needs to include their auth token as a parameter
  to the request, which is then used to validate that the user is at
  all allowed to establish a connection.

  The socket supports following topics:

  - __user__ - returns all task notifications for the given user, like results

  The connection will be established if the user is authenticated.
  Otherwise, a 403 response is returned.
  """
  use Phoenix.Socket
  require Logger

  # List of exposed channels
  channel("user_queries:*", AirWeb.Socket.Frontend.UserChannel)
  channel("type_check:*", AirWeb.Socket.Frontend.UserChannel)
  channel("query:*", AirWeb.Socket.Frontend.UserChannel)
  channel("state_changes:*", AirWeb.Socket.Frontend.UserChannel)
  channel("selectables:*", AirWeb.Socket.Frontend.UserChannel)
  channel("cloak_stats", AirWeb.Socket.Frontend.CloakStatsChannel)
  channel("data_source:*", AirWeb.Socket.Frontend.DataSourceChannel)

  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Socket
  def connect(%{"token" => token}, socket) do
    with {:ok, user_id} <- Air.Service.RevokableToken.verify(token, :session),
         {:ok, user} <- Air.Service.User.load_enabled(user_id) do
      {:ok, assign(socket, :user, user)}
    else
      _ -> :error
    end
  end

  @impl Phoenix.Socket
  def id(socket), do: "user:#{socket.assigns.user.id}"
end
