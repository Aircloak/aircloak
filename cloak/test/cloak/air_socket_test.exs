defmodule Cloak.AirSocketTest do
  # this suite uses a globally registered socket process, so it must run in isolation
  use ExUnit.Case, async: false

  alias Cloak.AirSocket
  alias Cloak.CloakSocketMock
  alias Cloak.CloakSocketMock.MainChannel

  setup_all do
    ExUnit.CaptureLog.capture_log(fn -> {:ok, _} = CloakSocketMock.Endpoint.start_link() end)
    :ok
  end

  setup do
    # create a unique cloak_name, to avoid name registration clashes
    cloak_name =
      :erlang.unique_integer()
      |> :erlang.term_to_binary()
      |> Base.encode64()
    # we'll keep the cloak_name in the procdict to avoid passing it around
    Process.put(:cloak_name, cloak_name)
    AirSocket.set_cloak_name(cloak_name)
    {:ok, _pid} = AirSocket.start_link()
    :ok
  end

  test "connection" do
    MainChannel.await(cloak_name())
    cloak_info = MainChannel.cloak_info(cloak_name())
    assert %{"data_sources" => [%{"id" => "local", "tables" => _}]} = cloak_info
  end

  test "disconnect/reconnect" do
    air_config = Application.get_env(:cloak, :air)
    Application.put_env(:cloak, :air, Keyword.put(air_config, :reconnect_interval, 10))
    try do
      ensure_joined()

      reg_name = MainChannel.reg_name(cloak_name())
      :gproc_monitor.subscribe(reg_name)

      ExUnit.CaptureLog.capture_log(fn ->
            CloakSocketMock.Socket.kill(cloak_name())
            assert_receive {:gproc_monitor, ^reg_name, :undefined}
            ensure_joined()
          end)
    after
      Application.put_env(:cloak, :air, air_config)
    end
  end

  test "rejoin the main channel" do
    air_config = Application.get_env(:cloak, :air)
    Application.put_env(:cloak, :air, Keyword.put(air_config, :rejoin_interval, 10))
    try do
      ensure_joined()

      reg_name = MainChannel.reg_name(cloak_name())
      :gproc_monitor.subscribe(reg_name)

      ExUnit.CaptureLog.capture_log(fn ->
            MainChannel.leave(cloak_name())
            assert_receive {:gproc_monitor, ^reg_name, :undefined}
            ensure_joined()
          end)
    after
      Application.put_env(:cloak, :air, air_config)
    end
  end

  test "starting a task" do
    ensure_joined()
    MainChannel.subscribe(cloak_name)
    request = %{request_id: "foo", event: "run_task", payload: %{id: 42, prefetch: [], code: ""}}
    MainChannel.send_to_cloak(cloak_name(), "air_call", request)
    assert_receive {:in_message, "call_response", response}
    assert %{"request_id" => "foo", "status" => "ok"} = response
    assert_receive {:in_message, "cloak_call", response}
    assert %{"event" => "task_results", "payload" => %{"task_id" => 42}} = response
  end

  test "sending a task result" do
    ensure_joined()
    MainChannel.subscribe(cloak_name)
    me = self()
    spawn(fn ->
          res = AirSocket.send_task_results(%{task_id: 1})
          send(me, {:send_task_results, res})
        end)
    assert_receive {:in_message, "cloak_call", response}
    assert%{"event" => "task_results", "payload" => %{"task_id" => 1}, "request_id" => request_id} = response

    response = %{request_id: request_id, status: "ok"}
    MainChannel.send_to_cloak(cloak_name(), "call_response", response)
    assert_receive {:send_task_results, :ok}
  end

  defp cloak_name, do: Process.get(:cloak_name)

  defp ensure_joined do
    MainChannel.await(cloak_name())
    assert %{} = MainChannel.cloak_info(cloak_name())
  end
end
