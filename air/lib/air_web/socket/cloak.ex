defmodule AirWeb.Socket.Cloak do
  @moduledoc """
  Implements websocket interface for cloaks.

  A cloak needs to provide its token as request query parameters in URL.

  The socket supports following topics:

  - __main__ - general purpose communication

  The connection will be established if the cloak is authenticated. Otherwise, a
  403 response is returned.
  """
  use Phoenix.Socket
  require Logger

  transport :websocket, Phoenix.Transports.WebSocket, serializer: [{AirWeb.Socket.Cloak.Serializer, "~> 2.0.0"}]

  # List of exposed channels
  channel "main", AirWeb.Socket.Cloak.MainChannel


  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Socket
  def connect(params, socket) do
    Logger.info("Cloak connecting #{inspect params}")
    cloak_name = params["cloak_name"]
    version = params["version"]
    if valid_required_param?(cloak_name) and valid_required_param?(version) do
      cloak_id = "#{cloak_name}"
      {:ok,
        socket
        |> assign(:cloak_id, cloak_id)
        |> assign(:version, version)
        |> assign(:name, cloak_name)
      }
    else
      Logger.info("Connection refused")
      :error
    end
  end

  @impl Phoenix.Socket
  def id(socket),
    do: "cloak_socket:#{socket.assigns.cloak_id}"


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp valid_required_param?(nil), do: false
  defp valid_required_param?(""), do: false
  defp valid_required_param?(_value), do: true
end
