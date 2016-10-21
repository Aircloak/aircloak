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

  transport :websocket, Phoenix.Transports.WebSocket, serializer: Central.Socket.Air.Serializer

  # List of exposed channels
  channel "main", Central.Socket.Air.MainChannel


  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @doc false
  def connect(params, socket) do
    Logger.info("Air connecting #{inspect params}")
    case values_from_params(params) do
      {:ok, token, air_name} ->
        case Customer.from_token(params["token"]) do
          {:ok, customer} ->
            socket = socket
              |> assign(:customer, customer)
              |> assign(:air_name, air_name)
            {:ok, socket}
          {:error, :invalid_token} ->
            Logger.info("Connection refused - invalid customer token")
            :error
        end
      :error ->
        Logger.info("Connection refused - incomplete parameters")
        :error
    end
  end

  @doc false
  def id(socket),
    do: "air_socket:#{socket.assigns.customer.id}"

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp values_from_params(%{"token" => token, "air_name" => air_name}), do: {:ok, token, air_name}
  defp values_from_params(_), do: :error
end
