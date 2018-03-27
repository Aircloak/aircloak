defmodule Cloak.LoggerTranslatorTest do
  use ExUnit.Case, async: false

  setup_all do
    Application.put_env(:cloak, :sanitize_otp_errors, true)
    on_exit(fn -> Application.put_env(:cloak, :sanitize_otp_errors, false) end)
  end

  describe "GenServer" do
    setup do
      {:ok, pid} = Agent.start(fn -> "sensitive data" end)
      [pid: pid]
    end

    test "stacktrace is logged on crash", %{pid: pid} do
      assert crash_agent(pid, fn _ -> raise "foo" end) =~
               ~r/#{Path.basename(__ENV__.file)}:#{__ENV__.line}/
    end

    test "sensitive data is removed from the exception message", %{pid: pid} do
      refute crash_agent(pid, fn _ -> Enum.count("sensitive data") end) =~ ~r/sensitive data/
    end

    test "sensitive data is removed from the stacktrace", %{pid: pid} do
      refute crash_agent(pid, fn _ -> List.last("sensitive data") end) =~ ~r/sensitive data/
    end

    defp crash_agent(pid, fun), do: capture_process_failure(pid, &Agent.cast(&1, fun))
  end

  describe "Task" do
    setup do
      {:ok, pid} = Task.start(__MODULE__, :task_loop, ["sensitive_data"])
      [pid: pid]
    end

    test "stacktrace is logged on crash", %{pid: pid} do
      assert crash_task(pid, fn _ -> raise "foo" end) =~
               ~r/#{Path.basename(__ENV__.file)}:#{__ENV__.line}/
    end

    test "sensitive data is removed from the exception message", %{pid: pid} do
      refute crash_task(pid, fn _ -> Enum.count("sensitive data") end) =~ ~r/sensitive data/
    end

    test "sensitive data is removed from the stacktrace", %{pid: pid} do
      refute crash_task(pid, fn _ -> List.last("sensitive data") end) =~ ~r/sensitive data/
    end

    defp crash_task(pid, fun), do: capture_process_failure(pid, &send(&1, {:fun, fun}))

    def task_loop(state) do
      receive do
        {:fun, fun} -> task_loop(fun.(state))
      end
    end
  end

  describe "other OTP process" do
    setup do
      {:ok, pid} = Task.start(__MODULE__, :start_task_loop, ["sensitive_data"])
      [pid: pid]
    end

    test "stacktrace is logged on crash", %{pid: pid} do
      assert crash_task(pid, fn _ -> raise "foo" end) =~
               ~r/#{Path.basename(__ENV__.file)}:#{__ENV__.line}/
    end

    test "sensitive data is removed from the exception message", %{pid: pid} do
      refute crash_task(pid, fn _ -> Enum.count("sensitive data") end) =~ ~r/sensitive data/
    end

    test "sensitive data is removed from the stacktrace", %{pid: pid} do
      refute crash_task(pid, fn _ -> List.last("sensitive data") end) =~ ~r/sensitive data/
    end

    def start_task_loop(arg) do
      :proc_lib.init_ack({:ok, self()})
      task_loop(arg)
    end
  end

  describe ":gen_event" do
    setup do
      {:ok, pid} = :gen_event.start()
      :gen_event.add_sup_handler(pid, Module.concat(__MODULE__, GenEventHandler), nil)

      [pid: pid]
    end

    test "stacktrace is logged on crash", %{pid: pid} do
      assert crash_event_handler(pid, fn _ -> raise "foo" end) =~
               ~r/#{Path.basename(__ENV__.file)}:#{__ENV__.line}/
    end

    test "sensitive data is removed from the exception message", %{pid: pid} do
      refute crash_event_handler(pid, fn _ -> Enum.count("sensitive data") end) =~
               ~r/sensitive data/
    end

    test "sensitive data is removed from the stacktrace", %{pid: pid} do
      refute crash_event_handler(pid, fn _ -> List.last("sensitive data") end) =~
               ~r/sensitive data/
    end

    defp crash_event_handler(pid, fun) do
      ExUnit.CaptureLog.capture_log(fn ->
        :gen_event.notify(pid, {:fun, fun})

        receive do
          {:gen_event_EXIT, _handler, _reason} -> :ok
        after
          :timer.seconds(1) -> raise "GenEvent handler didn't exit."
        end
      end)
    end

    defmodule GenEventHandler do
      @behaviour :gen_event

      def init(_), do: {:ok, nil}

      def handle_event({:fun, fun}, state), do: {:ok, fun.(state)}

      def handle_call(_request, _state), do: raise("not supported")
    end
  end

  defp capture_process_failure(pid, fun, timeout \\ :timer.seconds(1)) do
    ExUnit.CaptureLog.capture_log(fn ->
      mref = Process.monitor(pid)
      fun.(pid)

      receive do
        {:DOWN, ^mref, _, _, _} -> :ok
      after
        timeout -> raise "Process didn't exit"
      end
    end)
  end
end
