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

  alias Central.Service.Customer

  transport :websocket, Phoenix.Transports.WebSocket, serializer: [{Central.Socket.Air.Serializer, "~> 2.0.0"}]

  # List of exposed channels
  channel "main", Central.Socket.Air.MainChannel


  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Socket
  def connect(params, socket) do
    Logger.info("Air connecting #{inspect params}")
    with {:ok, token, air_name} <- values_from_params(params),
         {:ok, customer} <- Customer.from_token(token) do
      socket = socket
        |> assign(:customer, customer)
        |> assign(:air_name, air_name)
      {:ok, socket}
    else
      _ ->
        Logger.info("Connection refused - invalid params or customer token")
        :error
    end
  end

  @impl Phoenix.Socket
  def id(socket),
    do: "air_socket:#{socket.assigns.customer.id}"

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp values_from_params(%{"token" => token, "air_name" => air_name}), do: {:ok, token, air_name}
  defp values_from_params(_), do: :error
end
