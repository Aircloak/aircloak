defmodule Central.Utils.ProcessTest do
  use ExUnit.Case, async: true

  alias Central.Utils.Process, as: ProcessUtils

  test "alive? of a dead process" do
    pid = spawn(fn -> :timer.sleep(:infinity) end)
    Process.exit(pid, :kill)

    assert ProcessUtils.alive?(pid) == false
  end

  test "alive? of an alive process" do
    pid = spawn(fn -> :timer.sleep(:infinity) end)

    assert ProcessUtils.alive?(pid) == true
  end
end
