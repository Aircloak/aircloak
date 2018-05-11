defmodule IntegrationTest.CloakTest do
  use ExUnit.Case, async: false

  alias Cloak.AirSocket
  alias IntegrationTest.Manager

  setup do
    # trace cloak socket process
    :erlang.trace(Process.whereis(AirSocket), true, [:call])
    :erlang.trace_pattern({AirSocket, :_, :_}, [{:_, [], []}])

    # stop tracing on exit, since there can be only one tracer for the given target process
    on_exit(fn -> :erlang.trace(Process.whereis(AirSocket), false, [:call]) end)
  end

  @tag timeout: 120_000
  test "reconnects on broken connection" do
    # reduces flakiness and occasional test failures
    Process.sleep(:timer.seconds(1))

    ExUnit.CaptureLog.capture_log(fn ->
      # kill air socket process
      Manager.data_source_name()
      |> Air.Service.Cloak.channel_pids()
      |> hd()
      |> elem(0)
      |> Process.info()
      |> Keyword.fetch!(:dictionary)
      |> Keyword.fetch!(:"$ancestors")
      |> hd()
      |> Process.exit(:kill)

      # verify that cloak socket disconnects, reconnects, and rejoins the main channel
      assert_receive {:trace, _, :call, {AirSocket, :handle_disconnected, _}}, :timer.seconds(3)
      assert_receive {:trace, _, :call, {AirSocket, :handle_connected, _}}, :timer.seconds(3)
      assert_receive {:trace, _, :call, {AirSocket, :handle_joined, _}}, :timer.seconds(1)
      assert length(Air.Service.Cloak.channel_pids(Manager.data_source_name())) == 1
    end)
  end

  test "rejoins the main channel" do
    # reduces flakiness and occasional test failures
    Process.sleep(:timer.seconds(1))

    # trace cloak socket process
    :erlang.trace(Process.whereis(AirSocket), true, [:call])
    :erlang.trace_pattern({AirSocket, :_, :_}, [{:_, [], []}])

    ExUnit.CaptureLog.capture_log(fn ->
      # kill air channel process
      Air.Service.Cloak.channel_pids(Manager.data_source_name())
      |> hd()
      |> elem(0)
      |> Process.exit(:kill)

      # verify that cloak socket rejoins the main channel
      refute_receive {:trace, _, :call, {AirSocket, :handle_disconnected, _}}
      assert_receive {:trace, _, :call, {AirSocket, :handle_joined, _}}, :timer.seconds(3)
      assert length(Air.Service.Cloak.channel_pids(Manager.data_source_name())) == 1
    end)
  end
end
