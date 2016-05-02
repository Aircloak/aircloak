defmodule JsVmWorkerTest do
  use ExUnit.Case, async: true

  test "calling an undefined function" do
    assert {:ok, worker} = :js_vm_worker.start_link([])
    assert {:error, _} = :js_vm_worker.call(worker, "double", [21])
    GenServer.stop(worker)
  end

  test "calling a function" do
    assert {:ok, worker} = :js_vm_worker.start_link(["test/js/script.js"])
    assert {:ok, 42} == :js_vm_worker.call(worker, "double", [21])
    GenServer.stop(worker)
  end

  test "loading an invalid script" do
    Process.flag(:trap_exit, true)
    assert {:error, {reason, _}} = :js_vm_worker.start_link(["test/js/invalid_script.js"])
    assert {:badmatch, {:error, :enoent}} == reason
  end
end
