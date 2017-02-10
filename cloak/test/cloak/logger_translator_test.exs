defmodule Cloak.LoggerTranslatorTest do
  use ExUnit.Case, async: false

  describe "GenServer" do
    setup do
      {:ok, pid} = Agent.start(fn -> "sensitive data" end)
      [pid: pid]
    end

    test "stacktrace is logged on crash", %{pid: pid}, do:
      assert crash_agent(pid, fn(_) -> raise "foo" end) =~ ~r/#{Path.basename(__ENV__.file)}:#{__ENV__.line}/

    test "sensitive data is removed from the exception message", %{pid: pid}, do:
      refute crash_agent(pid, fn(_) -> Enum.count("sensitive data") end) =~ ~r/sensitive data/

    test "sensitive data is removed from the stacktrace", %{pid: pid}, do:
      refute crash_agent(pid, fn(_) -> List.last("sensitive data") end) =~ ~r/sensitive data/

    defp crash_agent(pid, fun), do:
      capture_process_failure(pid, &Agent.cast(&1, fun))
  end

  describe "Task" do
    setup do
      {:ok, pid} = Task.start(__MODULE__, :task_loop, ["sensitive_data"])
      [pid: pid]
    end

    test "stacktrace is logged on crash", %{pid: pid}, do:
      assert crash_task(pid, fn(_) -> raise "foo" end) =~ ~r/#{Path.basename(__ENV__.file)}:#{__ENV__.line}/

    test "sensitive data is removed from the exception message", %{pid: pid}, do:
      refute crash_task(pid, fn(_) -> Enum.count("sensitive data") end) =~ ~r/sensitive data/

    test "sensitive data is removed from the stacktrace", %{pid: pid}, do:
      refute crash_task(pid, fn(_) -> List.last("sensitive data") end) =~ ~r/sensitive data/

    defp crash_task(pid, fun), do:
      capture_process_failure(pid, &send(&1, {:fun, fun}))

    def task_loop(state) do
      receive do
        {:fun, fun} -> task_loop(fun.(state))
      end
    end
  end

  defp capture_process_failure(pid, fun, timeout \\ :timer.seconds(1)) do
    ExUnit.CaptureLog.capture_log(fn ->
      mref = Process.monitor(pid)
      fun.(pid)
      receive do
        {:DOWN, ^mref, _, _, _} -> :ok
        after timeout -> raise "Process didn't exit"
      end
    end)
  end
end
