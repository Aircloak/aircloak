defmodule Air.Service.Central.RpcQueueTest do
  use ExUnit.Case, async: true

  alias Air.Service.Central.RpcQueue

  setup_all do
    :ets.new(__MODULE__, [:public, :set, :named_table])
    :ok
  end

  defmacrop assert_next_rpc(expected_rpc) do
    quote do
      expected_rpc = unquote(expected_rpc)
      assert_receive {:rpc, rpc}, 1000
      assert rpc.event == expected_rpc.event
      assert rpc.payload == expected_rpc.payload
    end
  end

  test "sending a message" do
    pid = start_worker()
    rpc = push_rpc(pid, unique_rpc())

    assert_next_rpc(rpc)
  end

  test "message is sent only once on success" do
    pid = start_worker()
    push_rpc(pid, unique_rpc())

    assert_receive {:rpc, _}
    refute_receive {:rpc, _}
  end

  test "message is not sent if the socket is disconnected" do
    pid = start_worker(connected_fun: fn -> false end)
    push_rpc(pid, unique_rpc())

    refute_receive {:rpc, _}
  end

  test "message is sent after the socket is connected" do
    me = self()

    connected_fun = fn ->
      {:dictionary, dictionary} = Process.info(me, :dictionary)
      Keyword.fetch!(dictionary, :connected)
    end

    Process.put(:connected, false)
    pid = start_worker(connected_fun: connected_fun, retry_delay: 50)
    rpc = push_rpc(pid, unique_rpc())
    :timer.sleep(100)
    Process.put(:connected, true)

    assert_next_rpc(rpc)
  end

  test "proper message ordering" do
    pid = start_worker()

    [rpc1, rpc2, rpc3] =
      push_rpcs(pid, [
        unique_rpc(type: :delay, delay: 50),
        unique_rpc(type: :delay, delay: 5),
        unique_rpc()
      ])

    assert_next_rpc(rpc1)
    assert_next_rpc(rpc2)
    assert_next_rpc(rpc3)
  end

  test "removing items on queue overflow" do
    pid = start_worker(max_size: 2)

    [rpc1, _, rpc3, rpc4] =
      push_rpcs(pid, Enum.map(1..4, fn _ -> unique_rpc(type: :delay, delay: 50) end))

    assert_next_rpc(rpc1)
    assert_next_rpc(rpc3)
    assert_next_rpc(rpc4)

    refute_receive {:rpc, _}
  end

  test "no retry if item is removed on queue overflow" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(max_size: 2, retry_delay: 50)
      push_rpc(pid, unique_rpc(type: :error, num_errors: 1))

      [_, rpc3, rpc4] =
        push_rpcs(pid, Enum.map(1..3, fn _ -> unique_rpc(type: :delay, delay: 50) end))

      assert_next_rpc(rpc3)
      assert_next_rpc(rpc4)

      refute_receive {:rpc, _}
    end)
  end

  test "retry sending of an error message" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(retry_delay: 1)
      rpc = push_rpc(pid, unique_rpc(type: :error, num_errors: 10))

      assert_next_rpc(rpc)
      refute_receive {:rpc, _}
    end)
  end

  test "ordering is preserved on send error" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(retry_delay: 5)

      [rpc1, rpc2] =
        push_rpcs(pid, [
          unique_rpc(type: :error, num_errors: 5),
          unique_rpc()
        ])

      assert_next_rpc(rpc1)
      assert_next_rpc(rpc2)
    end)
  end

  defp push_rpc(pid, rpc_data) do
    [rpc] = push_rpcs(pid, [rpc_data])
    rpc
  end

  defp push_rpcs(pid, rpcs_data) do
    Enum.each(rpcs_data, &RpcQueue.push(pid, &1.event, &1.payload))
    Enum.map(rpcs_data, &Air.Service.Central.new_rpc(nil, &1.event, &1.payload))
  end

  defp start_worker(opts \\ []) do
    {:ok, pid} =
      RpcQueue.start_link(
        Keyword.merge(
          [name: nil, sender_fun: test_sender(), connected_fun: fn -> true end],
          opts
        )
      )

    pid
  end

  defp test_sender() do
    receiver = self()

    fn rpc ->
      case rpc.payload.type do
        :normal ->
          normal_send(receiver, rpc)

        :error ->
          if dec_error_counter(rpc.payload) >= 0,
            do: raise("error"),
            else: normal_send(receiver, rpc)

        :delay ->
          :timer.sleep(rpc.payload.delay)
          normal_send(receiver, rpc)
      end
    end
  end

  defp normal_send(receiver, rpc) do
    send(receiver, {:rpc, rpc})
    {:ok, nil}
  end

  defp unique_rpc(opts \\ []) do
    unique_piece = to_string(:erlang.unique_integer())

    %{
      event: unique_piece,
      payload: Map.new(Keyword.merge([id: unique_piece, type: :normal], opts))
    }
  end

  defp dec_error_counter(error_payload) do
    ets_key = {:error_counter, error_payload.id}
    :ets.update_counter(__MODULE__, ets_key, {2, -1}, {ets_key, error_payload.num_errors})
  end
end
