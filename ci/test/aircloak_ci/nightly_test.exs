defmodule AircloakCI.NightlyTest do
  use AircloakCI.BuildCase, async: false

  test "successful nightly job", %{repo_data: repo_data} do
    branch = hd(repo_data.branches)
    project = AircloakCI.LocalProject.for_branch(branch)
    wait_for_jobs_to_finish(branch)

    assert {:ok, job_data} = AircloakCI.Build.Nightly.force(project, "system_test", :system_test)
    assert job_data.project == project
    assert job_data.job_spec.component == "system_test"
    assert job_data.job_spec.job == :system_test

    mref = Process.monitor(job_data.pid)
    assert_receive({:DOWN, ^mref, :process, _pid, :normal})

    executed_job =
      :sys.get_state(AircloakCI.Build.Nightly).executed_jobs[{"branch master", "system_test", :system_test}]

    assert executed_job.date == Date.utc_today()
    assert executed_job.sha == branch.sha
    refute_receive {:commented_on_commit, _comment_data}
  end

  test "errored nightly job", %{repo_data: repo_data} do
    branch = hd(repo_data.branches)
    project = AircloakCI.LocalProject.for_branch(branch)
    wait_for_jobs_to_finish(branch)

    ExUnit.CaptureLog.capture_log(fn ->
      simulate_command(~r/make test/, fn -> exit({:exit_status, 1}) end)
      assert {:ok, job_data} = AircloakCI.Build.Nightly.force(project, "system_test", :system_test, branch)

      mref = Process.monitor(job_data.pid)
      assert_receive({:DOWN, ^mref, :process, _pid, :normal})
    end)

    assert_receive {:commented_on_commit, comment_data}, :timer.seconds(2)
    assert comment_data.sha == branch.sha
    assert comment_data.body =~ ~r/error running `.+ make test`/
  end
end
