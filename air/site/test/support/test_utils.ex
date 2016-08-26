defmodule Air.TestUtils do
  @moduledoc """
  Adds generic helpers useful in tests
  """

  @doc """
  Creates a temporary process with a terminator function that can be triggered
  to destroy it on demand. The temporary process is linked to the caller, and
  will therefore terminate at the latest along with the test case using it.

  Usage:

    {termination_callback, pid} = temporary_process()
    # ... use pid in whatever process needs something to monitor
    # then invoke the terminator once done in order to validate
    # the correct behaviuor.
    termination_callback.()

  Frequently useful in conjuction with the `soon` assertion:

  Usage:

    require Air.AssertionHelper

    test "..." do
      {termination_callback, pid} = temporary_process()
      ModuleUnderTest.process_pid(pid)
      termination_callback.()
      assert soon(ModuleUnderTest.detected_process_failure())
    end
  """
  def temporary_process() do
    pid = spawn_link(fn -> receive do :stop -> :ok end end)
    {fn -> send(pid, :stop) end, pid}
  end
end
