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
    {:ok, socket} = TestSocket.start_link(GenSocketClient.Transport.WebSocketClient, url(params), true,
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

  @doc "Awaits run_query request and responds with a given status."
  @spec respond_to_start_task_request!(pid, String.t, String.t) :: :ok
  def respond_to_start_task_request!(socket, task_id, status) do
    timeout = :timer.seconds(1)
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    %{"event" => "run_query", "payload" => %{"id" => ^task_id}, "request_id" => request_id} = request
    {:ok, _ref} = TestSocket.push(socket, "main", "call_response", %{request_id: request_id, status: status})
    :ok
  end

  @doc "Awaits run_query request with any task_id and responds with the given status."
  @spec respond_to_start_task_request!(pid, String.t) :: :ok
  def respond_to_start_task_request!(socket, status) do
    timeout = :timer.seconds(1)
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    %{"event" => "run_query", "request_id" => request_id} = request
    {:ok, _ref} = TestSocket.push(socket, "main", "call_response", %{request_id: request_id, status: status})
    :ok
  end

  @doc "Runs the action while a air with the given name and data source exists and returns its result."
  @spec with_air(String.t, String.t, (() -> any)) :: any
  def with_air(air_name, data_source_name, action) do
    socket = connect!(%{air_name: air_name})

    try do
      data_source = %{global_id: data_source_name, tables: []}
      join!(socket, "main", %{name: air_name, data_sources: [data_source]})
      action.()
    after
      TestSocket.leave(socket, "main")
    end
  end

  defp url(params) do
    "#{Central.Endpoint.url}/air/socket/websocket?#{URI.encode_query(params)}"
    |> String.replace(~r(http://), "ws://")
    |> String.replace(~r(https://), "wss://")
  end
end
