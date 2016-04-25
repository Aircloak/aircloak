defmodule Air.TestSocketHelper do
  @moduledoc """
  Helper for mocking the cloak socket.

  This module can be used in various tests where the controller handler needs to work with a socket.
  Using functions from this module, you can create a mock cloak socket, and interact with the controller
  function.
  """
  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket

  @doc "Opens a socket and waits for the connection to be established."
  @spec connect!(%{}) :: GenServer.on_start
  def connect!(params \\ %{cloak_name: "cloak_1"}) do
    {:ok, socket} = TestSocket.start_link(GenSocketClient.Transport.WebSocketClient, url(params), true,
        serializer: GenSocketClient.Serializer.GzipJson)
    :connected = TestSocket.wait_connect_status(socket)
    socket
  end

  @doc "Joins a topic and wait for the successful response."
  @spec join!(pid, String.t, %{}) :: {:ok, %{}}
  def join!(socket, topic, params) do
    {:ok, {^topic, response}} = TestSocket.join(socket, topic, params)
    {:ok, response}
  end

  @doc "Awaits run_task request and responds with a given status."
  @spec respond_to_start_task_request!(pid, String.t, String.t, pos_integer) :: :ok
  def respond_to_start_task_request!(socket, task_id, status, timeout \\ :timer.seconds(1)) do
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    %{"event" => "run_task", "payload" => %{"id" => ^task_id}, "request_id" => request_id} = request
    {:ok, _ref} = TestSocket.push(socket, "main", "call_response", %{request_id: request_id, status: status})
    :ok
  end

  defp url(params) do
    "#{Air.Endpoint.url}/cloak/socket/websocket?#{URI.encode_query(params)}"
    |> String.replace(~r(http://), "ws://")
    |> String.replace(~r(https://), "wss://")
  end
end
