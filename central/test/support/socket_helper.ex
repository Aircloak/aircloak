defmodule Central.TestSocketHelper do
  @moduledoc """
  Helper for mocking the air socket.

  This module can be used in various tests where the controller handler needs to work with a socket.
  Using functions from this module, you can create a mock air socket, and interact with the controller
  function.
  """
  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket

  @doc "Opens a socket and waits for the connection status."
  @spec connect!(%{}) :: {status::any, GenServer.on_start}
  def connect(params) do
    {:ok, socket} = TestSocket.start_link(GenSocketClient.Transport.WebSocketClient, url(), Enum.to_list(params), true,
        serializer: GenSocketClient.Serializer.GzipJson)
    {TestSocket.wait_connect_status(socket), socket}
  end

  @doc "Opens a socket and waits for the connection to be successfully established."
  @spec connect!(%{}) :: GenServer.on_start
  def connect!(params) do
    {:connected, socket} = connect(params)
    socket
  end

  @doc "Joins a topic and wait for the successful response."
  @spec join!(pid, String.t, %{}) :: {:ok, %{}}
  def join!(socket, topic, params \\ %{}) do
    {:ok, {^topic, response}} = TestSocket.join(socket, topic, params)
    {:ok, response}
  end

  @doc "Runs the action while a air with the given name and data source exists and returns its result."
  @spec with_air(Map.t, (() -> any)) :: any
  def with_air(params, action) do
    socket = connect!(params)

    try do
      join!(socket, "main", %{})
      action.()
    after
      TestSocket.leave(socket, "main")
    end
  end

  defp url() do
    "#{Central.Endpoint.url}/air/socket/websocket"
    |> String.replace(~r(http://), "ws://")
    |> String.replace(~r(https://), "wss://")
  end
end
