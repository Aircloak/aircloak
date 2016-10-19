defmodule Central.Socket.Air do
  @moduledoc """
  Implements websocket interface for airs.

  An air needs to provide its token as request query parameters in URL.

  The socket supports following topics:

  - __main__ - general purpose communication

  The connection will be established if the air is authenticated. Otherwise, a
  403 response is returned.
  """
  use Phoenix.Socket
  require Logger

  transport :websocket, Phoenix.Transports.WebSocket, serializer: Central.Socket.Air.Serializer

  # List of exposed channels
  channel "main", Central.Socket.Air.MainChannel


  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @doc false
  def connect(params, socket) do
    Logger.info("Air connecting #{inspect params}")
    air_name = params["air_name"]
    if valid_required_param?(air_name) do
      air_id = "#{air_name}"
      {:ok,
        socket
        |> assign(:air_id, air_id)
        |> assign(:name, air_name)
      }
    else
      Logger.info("Connection refused")
      :error
    end
  end

  @doc false
  def id(socket),
    do: "air_socket:#{socket.assigns.air_id}"


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp valid_required_param?(nil), do: false
  defp valid_required_param?(""), do: false
  defp valid_required_param?(_value), do: true
end
