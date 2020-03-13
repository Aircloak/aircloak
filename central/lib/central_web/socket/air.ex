defmodule CentralWeb.Socket.Air do
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

  alias Central.Service.{Customer, License}

  # List of exposed channels
  channel("main", CentralWeb.Socket.Air.MainChannel)

  # -------------------------------------------------------------------
  # Phoenix.Socket callback functions
  # -------------------------------------------------------------------

  @impl Phoenix.Socket
  def connect(params, socket) do
    Logger.info("Air connecting #{inspect(params)}")

    case customer_from_params(params) do
      {:ok, customer, air_name} ->
        socket =
          socket
          |> assign(:customer, customer)
          |> assign(:air_name, air_name)

        {:ok, socket}

      _ ->
        Logger.info("Connection refused - invalid params or customer token")
        :error
    end
  end

  @impl Phoenix.Socket
  def id(socket), do: "air_socket:#{socket.assigns.customer.id}"

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp customer_from_params(%{"token" => token, "air_name" => air_name}),
    do: with({:ok, customer} <- Customer.from_token(token), do: {:ok, customer, air_name})

  defp customer_from_params(%{"license" => license, "air_name" => air_name}) do
    with {:ok, license} <- License.decrypt(license),
         {:ok, customer} <- Customer.from_license(license) do
      {:ok, customer, air_name}
    end
  end

  defp customer_from_params(_), do: :error
end
