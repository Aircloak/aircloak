defmodule Air.Socket.Cloak do
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

  transport :websocket, Phoenix.Transports.WebSocket, serializer: Air.Socket.Cloak.Serializer

  # List of exposed channels
  channel "main", Air.Socket.Cloak.MainChannel


  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @doc false
  def connect(params, socket) do
    Logger.info("Cloak connecting #{inspect params}")
    cloak_name = params["cloak_name"]
    cloak_organisation = params["cloak_organisation"]
    if valid_required_param?(cloak_name) && valid_required_param?(cloak_organisation) do
      cloak_id = "#{cloak_organisation}/#{cloak_name}"
      {:ok,
        socket
        |> assign(:cloak_id, cloak_id)
        |> assign(:name, cloak_name)
        |> assign(:organisation, cloak_organisation)
      }
    else
      Logger.info("Connection refused")
      :error
    end
  end

  @doc false
  def id(socket),
    do: "cloak_socket:#{socket.assigns.cloak_id}"


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp valid_required_param?(nil), do: false
  defp valid_required_param?(""), do: false
  defp valid_required_param?(_value), do: true
end
