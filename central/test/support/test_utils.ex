defmodule Central.TestUtils do
  @moduledoc """
  Adds generic helpers useful in tests
  """

  use ExUnit.CaseTemplate

  # -------------------------------------------------------------------
  # Helper functions
  # -------------------------------------------------------------------

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

    require Central.AssertionHelper

    test "..." do
      {termination_callback, pid} = temporary_process()
      ModuleUnderTest.process_pid(pid)
      termination_callback.()
      assert soon(ModuleUnderTest.detected_process_failure())
    end
  """
  def temporary_process() do
    pid =
      spawn_link(fn ->
        receive do
          :stop -> :ok
        end
      end)

    {fn -> send(pid, :stop) end, pid}
  end

  @doc """
  Validates that the change in the count of join table entries differs
  by a set amount after invoking a function.
  """
  def assert_join_table_count_change(change, function) do
    count_before = join_table_count()
    result = function.()
    count_after = join_table_count()
    assert change == count_after - count_before
    result
  end

  @doc "Asserts that the join table has a certain count"
  def assert_join_table_count(expected_count) do
    assert expected_count == join_table_count()
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp join_table_count() do
    %{rows: [[actual_count_groups_users]]} =
      Ecto.Adapters.SQL.query!(
        Central.Repo,
        """
          SELECT count(*) FROM groups_users
        """,
        []
      )

    %{rows: [[actual_count_data_sources_groups]]} =
      Ecto.Adapters.SQL.query!(
        Central.Repo,
        """
          SELECT count(*) FROM data_sources_groups
        """,
        []
      )

    actual_count_groups_users + actual_count_data_sources_groups
  end
end
