defmodule Air.Socket.Cloak do
  @moduledoc """
  Implements websocket interface for cloaks.

  A cloak needs to provide following parameters (as request query parameters in URL):

  - shared_secret
  - cloak_token

  The socket supports following topics:

  - __main__ - general purpose communication

  The connection will be established if the cloak is authenticated. Otherwise, a
  403 response is returned.
  """
  use Phoenix.Socket

  transport :websocket, Phoenix.Transports.WebSocket

  # List of exposed channels
  channel "main", Air.Socket.Cloak.MainChannel


  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @doc false
  def connect(params, socket) do
    shared_secret = :air_etcd.get("/service/airpub/shared_secret")
    cond do
      params["shared_secret"] == shared_secret and params["cloak_token"] != nil ->
        {:ok, assign(socket, :cloak_token, params["cloak_token"])}
      true -> :error
    end
  end

  @doc false
  def id(socket),
    do: "cloak_socket:#{socket.assigns.cloak_token}"
end
