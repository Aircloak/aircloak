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
    params = %{cloak_name: cloak_name}
    {:ok, socket_pid} = AirSocket.start_link(params, [])
    {:ok, %{socket_pid: socket_pid, cloak_name: cloak_name}}
  end

  test "connection", %{cloak_name: cloak_name} do
    MainChannel.await(cloak_name)
    cloak_info = MainChannel.cloak_info(cloak_name)
    assert %{"data_sources" => [%{"global_id" => _, "tables" => _} | _rest]} = cloak_info
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

  defp ensure_joined(cloak_name) do
    MainChannel.await(cloak_name)
    assert %{} = MainChannel.cloak_info(cloak_name)
  end
end
