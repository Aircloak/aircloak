defmodule Cloak.LoggerTranslatorTest do
  use ExUnit.Case, async: false

  test "crashing GenServer" do
    {:ok, pid} = Agent.start(fn -> nil end)
    assert capture_process_failure(pid, &Agent.cast(&1, fn(_) -> raise "bar" end)) == ""
  end

  defp capture_process_failure(pid, fun) do
    ExUnit.CaptureLog.capture_log(fn ->
      mref = Process.monitor(pid)
      fun.(pid)
      receive do {:DOWN, ^mref, _, _, _} -> :ok end
    end)
  end
end
