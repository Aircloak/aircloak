defmodule Air.TestSocketHelper do
  @moduledoc """
  Helper for mocking the cloak socket.

  This module can be used in various tests where the controller handler needs to work with a socket.
  Using functions from this module, you can create a mock cloak socket, and interact with the controller
  function.
  """
  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket

  @doc "Opens a socket and waits for the connection status."
  @spec connect!(%{}) :: {status::any, GenServer.on_start}
  def connect(params) do
    params = case params["version"] do
      nil ->
        # We default to the current version. Tests that explicitly
        # set the version are likely to want to test connection
        # failure upon a wrong version number
        Map.put(params, "version", File.read!("../VERSION"))
      _ -> params
    end
    {:ok, socket} = TestSocket.start_link(GenSocketClient.Transport.WebSocketClient, url(), Enum.to_list(params), true,
        serializer: Air.CloakSocketSerializer)
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
    params = Map.merge(%{salt_hash: "foobar"}, params)
    {:ok, {^topic, response}} = TestSocket.join(socket, topic, params)
    {:ok, response}
  end

  @doc "Awaits run_query request and responds with a given status."
  @spec respond_to_start_task_request!(pid, String.t, String.t) :: :ok
  def respond_to_start_task_request!(socket, task_id, status) do
    timeout = :timer.seconds(1)
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    %{event: "run_query", payload: %{id: ^task_id}, request_id: request_id} = request
    {:ok, _ref} = TestSocket.push(socket, "main", "cloak_response", %{request_id: request_id, status: status})
    :ok
  end

  @doc "Awaits run_query request with any task_id and responds with the given status."
  @spec respond_to_start_task_request!(pid, String.t) :: :ok
  def respond_to_start_task_request!(socket, status) do
    timeout = :timer.seconds(1)
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    %{event: "run_query", request_id: request_id} = request
    {:ok, _ref} = TestSocket.push(socket, "main", "cloak_response", %{request_id: request_id, status: status})
    :ok
  end

  @doc "Awaits an is_alive request with the given query_id and responds with the given result."
  @spec respond_to_running_queries!(pid, [String.t], pos_integer) :: :ok
  def respond_to_running_queries!(socket, result, timeout \\ :timer.seconds(1)) do
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    %{request_id: request_id, event: "running_queries", payload: nil} = request
    {:ok, _ref} = TestSocket.push(socket, "main", "cloak_response", %{
      request_id: request_id, status: :ok, result: result})
    :ok
  end

  @doc "Awaits a validate_views request with the given query_id and responds with the given result."
  @spec respond_to_validate_views!(pid, [map], pos_integer) :: :ok
  def respond_to_validate_views!(socket, results, timeout \\ :timer.seconds(1)) do
    {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, timeout)
    %{request_id: request_id, event: "validate_views", payload: _} = request
    {:ok, _ref} = TestSocket.push(socket, "main", "cloak_response", %{
      request_id: request_id, status: :ok, result: results})
    :ok
  end

  @doc "Runs the action while a cloak with the given name and data source exists and returns its result."
  @spec with_cloak(String.t, String.t, (() -> any)) :: any
  def with_cloak(cloak_name, data_source_name, action) do
    socket = connect!(%{cloak_name: cloak_name})

    try do
      data_source = %{name: data_source_name, tables: []}
      join!(socket, "main", %{name: cloak_name, data_sources: [data_source]})
      action.()
    after
      TestSocket.leave(socket, "main")
    end
  end

  defp url() do
    "#{AirWeb.Endpoint.url}/cloak/socket/websocket"
    |> String.replace(~r(http://), "ws://")
    |> String.replace(~r(https://), "wss://")
  end
end
