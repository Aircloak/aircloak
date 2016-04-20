defmodule Air.Socket.CloakTest do
  use ExUnit.Case, async: true

  alias Phoenix.Channels.GenSocketClient
  alias GenSocketClient.TestSocket
  alias Air.Socket.Cloak.MainChannel


  test "invalid authentication" do
    assert {:ok, socket} = start_link(url(%{}))
    assert {:disconnected, {403, "Forbidden"}} == TestSocket.wait_connect_status(socket)
  end

  test "unmatched topic" do
    assert {:ok, socket} = start_link(url())
    assert :connected == TestSocket.wait_connect_status(socket)
    assert {:error, reason} = TestSocket.join(socket, "invalid_channel")
    assert {:server_rejected, "invalid_channel", %{"reason" => "unmatched topic"}} == reason
  end

  test "main topic" do
    assert {:ok, socket} = start_link(url())
    assert :connected == TestSocket.wait_connect_status(socket)
    assert {:ok, {"main", %{}}} == join_main_channel(socket)
  end

  test "starting a task" do
    assert {:ok, socket} = start_link(url())
    assert :connected == TestSocket.wait_connect_status(socket)
    assert {:ok, {"main", %{}}} == join_main_channel(socket)

    me = self()
    spawn(fn ->
          start_task_result = MainChannel.run_task("unknown_org/cloak_1", %{id: 42})
          send(me, {:start_task_result, start_task_result})
        end)
    assert {:ok, {"main", "air_call", request}} = TestSocket.await_message(socket, 100)
    assert %{"event" => "run_task", "payload" => %{"id" => 42}, "request_id" => request_id} = request

    TestSocket.push(socket, "main", "call_response", %{request_id: request_id, status: "ok"})
    assert_receive {:start_task_result, :ok}
  end

  test "receiving a task result" do
    assert {:ok, socket} = start_link(url())
    assert :connected == TestSocket.wait_connect_status(socket)
    assert {:ok, {"main", %{}}} == join_main_channel(socket)

    request = %{request_id: "foobar", event: "task_results", payload: [1, 2, 3]}
    TestSocket.push(socket, "main", "cloak_call", request)
    assert {:ok, {"main", "call_response", response}} = TestSocket.await_message(socket, 100)
    assert %{"request_id" => "foobar", "status" => "ok"} = response
  end

  test "getting a list of connected cloaks" do
    assert {:ok, socket1} = start_link(url(%{cloak_name: "cloak_1"}))
    assert :connected == TestSocket.wait_connect_status(socket1)
    assert {:ok, {"main", %{}}} == join_main_channel(socket1, "cloak_1")
    assert [%Air.CloakInfo{name: "cloak_1"}] = MainChannel.connected

    assert {:ok, socket2} = start_link(url(%{cloak_name: "cloak_2"}))
    assert :connected == TestSocket.wait_connect_status(socket2)
    assert {:ok, {"main", %{}}} == join_main_channel(socket2, "cloak_2")
    assert [%Air.CloakInfo{name: "cloak_1"}, %Air.CloakInfo{name: "cloak_2"}] = MainChannel.connected

    mref = Process.monitor(MainChannel.channel_pid("unknown_org/cloak_1"))
    TestSocket.leave(socket1, "main")
    assert_receive {:DOWN, ^mref, _, _, _}
    assert [%Air.CloakInfo{name: "cloak_2"}] = MainChannel.connected
  end

  defp start_link(url) do
    TestSocket.start_link(GenSocketClient.Transport.WebSocketClient, url, true,
        serializer: GenSocketClient.Serializer.GzipJson)
  end

  defp url(params \\ %{
        cloak_name: "cloak_1"
      }) do
    "#{Air.Endpoint.url}/cloak/socket/websocket?#{URI.encode_query(params)}"
    |> String.replace(~r(http://), "ws://")
    |> String.replace(~r(https://), "wss://")
  end

  defp join_main_channel(socket, cloak_name \\ "cloak_1", data_sources \\ []) do
    TestSocket.join(socket, "main", %{name: cloak_name, data_sources: data_sources})
  end
end
