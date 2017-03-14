defmodule Air.Service.Central.CallsQueueTest do
  use ExUnit.Case, async: true

  alias Air.Service.Central.CallsQueue
  alias Air.Schemas.CentralCall

  setup_all do
    :ets.new(__MODULE__, [:public, :set, :named_table])
    :ok
  end

  defmacrop assert_next_rpc(expected_rpc) do
    quote do
      assert_receive {:rpc, rpc}, 1000
      assert rpc == unquote(expected_rpc)
    end
  end

  test "sending a message" do
    pid = start_worker()
    rpc = perform_rpc(pid, unique_call())

    assert_next_rpc rpc
  end

  test "message is sent only once on success" do
    pid = start_worker()
    CallsQueue.perform_rpc(pid, unique_call())

    assert_receive {:rpc, _}
    refute_receive {:rpc, _}
  end

  test "proper message ordering" do
    pid = start_worker()
    [rpc1, rpc2, rpc3] = perform_rpcs(pid, [
      unique_call(type: :delay, delay: 50),
      unique_call(type: :delay, delay: 5),
      unique_call()
    ])

    assert_next_rpc(rpc1)
    assert_next_rpc(rpc2)
    assert_next_rpc(rpc3)
  end

  test "queue size" do
    pid = start_worker()
    CallsQueue.perform_rpc(pid, unique_call(type: :delay, delay: 50))

    assert :sys.get_state(pid).pending_count == 1
    assert_receive {:rpc, _}
    assert :sys.get_state(pid).pending_count == 0
  end

  test "removing items on queue overflow" do
    pid = start_worker(max_size: 2)
    [rpc1, _, _, rpc4] = perform_rpcs(pid, Enum.map(1..4, fn _ -> unique_call(type: :delay, delay: 50) end))

    assert_next_rpc(rpc1)
    assert_next_rpc(rpc4)

    refute_receive {:rpc, _}
  end

  test "error message is not sent" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(retry_delay: :timer.hours(1))
      CallsQueue.perform_rpc(pid, unique_call(type: :error, num_errors: 1))
      refute_receive {:rpc, _}
    end)
  end

  test "retry sending of an error message" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(retry_delay: 1)
      rpc = perform_rpc(pid, unique_call(type: :error, num_errors: 10))

      assert_next_rpc rpc
      refute_receive {:rpc, _}
    end)
  end

  test "ordering is preserved on send error" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(retry_delay: 5)
      [rpc1, rpc2] = perform_rpcs(pid, [
        unique_call(type: :error, num_errors: 5),
        unique_call()
      ])

      assert_next_rpc rpc1
      assert_next_rpc rpc2
    end)
  end

  defp perform_rpc(pid, call) do
    [rpc] = perform_rpcs(pid, [call])
    rpc
  end

  defp perform_rpcs(pid, calls) do
    Enum.each(calls, &CallsQueue.perform_rpc(pid, &1))
    Enum.map(calls, &CentralCall.export/1)
  end

  defp start_worker(opts \\ []) do
    {:ok, pid} = CallsQueue.start_link(Keyword.merge([name: nil, sender_fun: test_sender()], opts))
    pid
  end

  defp test_sender() do
    receiver = self()
    fn(rpc) ->
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

  defp unique_call(opts \\ []) do
    payload = Map.new(Keyword.merge([id: :erlang.unique_integer(), type: :normal], opts))
    CentralCall.new(Base.encode64(:crypto.strong_rand_bytes(8)), payload)
  end

  defp dec_error_counter(error_payload) do
    ets_key = {:error_counter, error_payload.id}
    :ets.update_counter(__MODULE__, ets_key, {2, -1}, {ets_key, error_payload.num_errors})
  end
end
