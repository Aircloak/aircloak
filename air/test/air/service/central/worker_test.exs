defmodule Air.Service.Central.WorkerTest do
  use ExUnit.Case, async: true

  alias Air.Service.Central.Worker
  alias Air.Schemas.CentralCall

  setup_all do
    :ets.new(__MODULE__, [:public, :set, :named_table])
    :ok
  end

  test "sending a message" do
    pid = start_worker()
    call = unique_call()
    Worker.perform_rpc(pid, call)

    assert_receive {:rpc, rpc}
    assert rpc == CentralCall.export(call)
  end

  test "message is sent only once on success" do
    pid = start_worker()
    Worker.perform_rpc(pid, unique_call())

    assert_receive {:rpc, _}
    refute_receive {:rpc, _}
  end

  test "proper message ordering" do
    pid = start_worker()
    call1 = unique_call(type: :delay, delay: 50)
    call2 = unique_call(type: :delay, delay: 5)
    call3 = unique_call()
    Worker.perform_rpc(pid, call1)
    Worker.perform_rpc(pid, call2)
    Worker.perform_rpc(pid, call3)

    assert_receive {:rpc, rpc1}
    assert rpc1 == CentralCall.export(call1)

    assert_receive {:rpc, rpc2}
    assert rpc2 == CentralCall.export(call2)

    assert_receive {:rpc, rpc3}
    assert rpc3 == CentralCall.export(call3)
  end

  test "queue size" do
    pid = start_worker()
    Worker.perform_rpc(pid, unique_call(type: :delay, delay: 50))

    assert :sys.get_state(pid).pending_count == 1
    assert_receive {:rpc, _}
    assert :sys.get_state(pid).pending_count == 0
  end

  test "error message is not sent" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(central_retry_delay: :timer.hours(1))
      Worker.perform_rpc(pid, unique_call(type: :error, num_errors: 1))
      refute_receive {:rpc, _}
    end)
  end

  test "retry sending of an error message" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(central_retry_delay: 1)
      call = unique_call(type: :error, num_errors: 10)
      Worker.perform_rpc(pid, call)

      assert_receive {:rpc, rpc}, 1000
      assert rpc == CentralCall.export(call)
      refute_receive {:rpc, _}
    end)
  end

  test "ordering is preserved on send error" do
    ExUnit.CaptureLog.capture_log(fn ->
      pid = start_worker(central_retry_delay: 5)
      call1 = unique_call(type: :error, num_errors: 5)
      call2 = unique_call()
      Worker.perform_rpc(pid, call1)
      Worker.perform_rpc(pid, call2)

      assert_receive {:rpc, rpc1}, 1000
      assert rpc1 == CentralCall.export(call1)

      assert_receive {:rpc, rpc2}
      assert rpc2 == CentralCall.export(call2)
    end)
  end

  defp start_worker(opts \\ []) do
    {:ok, pid} = Worker.start_link(Keyword.merge([name: nil, sender_fun: test_sender()], opts))
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
