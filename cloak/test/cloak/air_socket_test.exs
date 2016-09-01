defmodule Cloak.AirSocketTest do
  use ExUnit.Case, async: true

  alias Cloak.AirSocket
  alias Cloak.CloakSocketMock
  alias Cloak.CloakSocketMock.MainChannel

  setup_all do
    ExUnit.CaptureLog.capture_log(fn -> {:ok, _} = CloakSocketMock.Endpoint.start_link() end)
    :ok
  end

  setup do
    # create a unique cloak_name, to avoid name registration clashes
    cloak_name = "cloak_#{:erlang.unique_integer()}"
    params = %{
      cloak_name: cloak_name,
      cloak_organisation: "test"
    }
    {:ok, socket_pid} = AirSocket.start_link(params, [])
    {:ok, %{socket_pid: socket_pid, cloak_name: cloak_name}}
  end

  test "connection", %{cloak_name: cloak_name} do
    MainChannel.await(cloak_name)
    cloak_info = MainChannel.cloak_info(cloak_name)
    assert %{"data_sources" => [%{"id" => _, "name" => _, "tables" => _} | _rest]} = cloak_info
  end

  test "disconnect/reconnect", %{cloak_name: cloak_name} do
    air_config = Application.get_env(:cloak, :air)
    Application.put_env(:cloak, :air, Keyword.put(air_config, :reconnect_interval, 10))
    try do
      ensure_joined(cloak_name)

      reg_name = MainChannel.reg_name(cloak_name)
      :gproc_monitor.subscribe(reg_name)

      ExUnit.CaptureLog.capture_log(fn ->
        CloakSocketMock.Socket.kill(cloak_name)
        assert_receive {:gproc_monitor, ^reg_name, :undefined}
        ensure_joined(cloak_name)
      end)
    after
      Application.put_env(:cloak, :air, air_config)
    end
  end

  test "rejoin the main channel", %{cloak_name: cloak_name} do
    air_config = Application.get_env(:cloak, :air)
    Application.put_env(:cloak, :air, Keyword.put(air_config, :rejoin_interval, 10))
    try do
      ensure_joined(cloak_name)

      reg_name = MainChannel.reg_name(cloak_name)
      :gproc_monitor.subscribe(reg_name)

      ExUnit.CaptureLog.capture_log(fn ->
        MainChannel.leave(cloak_name)
        assert_receive {:gproc_monitor, ^reg_name, :undefined}
        ensure_joined(cloak_name)
      end)
    after
      Application.put_env(:cloak, :air, air_config)
    end
  end

  test "starting a query", %{socket_pid: socket_pid, cloak_name: cloak_name} do
    Cloak.Test.DB.create_table("heights_as", "height INTEGER")

    Process.register(socket_pid, AirSocket)
    ensure_joined(cloak_name)
    MainChannel.subscribe(cloak_name)
    request = %{
      request_id: "foo",
      event: "run_query",
      payload: %{id: 42, statement: "SELECT height FROM cloak_test.heights_as", data_source: "local"}
    }
    MainChannel.send_to_cloak(cloak_name, "air_call", request)
    assert_receive {:in_message, "call_response", response}
    assert %{"request_id" => "foo", "status" => "ok"} = response
    assert_receive {:in_message, "cloak_call", response}, :timer.seconds(1)
    assert %{"event" => "query_result", "payload" => %{"query_id" => 42}} = response
    Process.unregister(AirSocket)
  end

  test "sending a query result", %{socket_pid: socket_pid, cloak_name: cloak_name} do
    ensure_joined(cloak_name)
    MainChannel.subscribe(cloak_name)
    me = self()
    spawn(fn ->
      res = AirSocket.send_query_result(socket_pid, %{query_id: 1})
      send(me, {:send_query_result, res})
    end)
    assert_receive {:in_message, "cloak_call", response}
    assert%{"event" => "query_result", "payload" => %{"query_id" => 1}, "request_id" => request_id} = response

    response = %{request_id: request_id, status: "ok"}
    MainChannel.send_to_cloak(cloak_name, "call_response", response)
    assert_receive {:send_query_result, :ok}
  end

  defp ensure_joined(cloak_name) do
    MainChannel.await(cloak_name)
    assert %{} = MainChannel.cloak_info(cloak_name)
  end
end
