defmodule AirWeb.Socket.Frontend do
  @moduledoc """
  Implements websocket interface for web clients using our interface

  The client needs to include their guardian auth token as a parameter
  to the request, which is then used to validate that the user is at
  all allowed to establish a connection.

  The socket supports following topics:

  - __user__ - returns all task notifications for the given user, like results

  The connection will be established if the user is authenticated.
  Otherwise, a 403 response is returned.
  """
  use Phoenix.Socket
  require Logger

  transport(:websocket, Phoenix.Transports.WebSocket)
  transport(:longpoll, Phoenix.Transports.LongPoll)

  # List of exposed channels
  channel("user_queries:*", AirWeb.Socket.Frontend.UserChannel)
  channel("query:*", AirWeb.Socket.Frontend.UserChannel)
  channel("state_changes:*", AirWeb.Socket.Frontend.UserChannel)
  channel("cloak_stats", AirWeb.Socket.Frontend.CloakStatsChannel)
  channel("data_source:*", AirWeb.Socket.Frontend.DataSourceChannel)

  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Socket
  def connect(%{"token" => token}, socket) do
    case Air.Guardian.decode_and_verify(token) do
      {:ok, claims} ->
        case Air.Guardian.resource_from_claims(claims) do
          {:ok, %Air.Schemas.User{} = user} -> {:ok, assign(socket, :user, user)}
          {:error, _reason} -> :error
        end

      {:error, _reason} ->
        :error
    end
  end

  @impl Phoenix.Socket
  def id(socket), do: "user:#{socket.assigns.user.id}"
end
