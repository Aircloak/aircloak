defmodule AircloakCI.PullRequestTest do
  use AircloakCI.BuildCase, async: false

  test "successful build", %{repo_data: repo_data} do
    pr = pr(approved?: true)
    repo_data = add_pr(repo_data, pr)
    AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)

    assert_posted_status(pr, "mergeable", %{
      state: :success,
      description: "pull request can be merged"
    })

    assert_pr_comment(pr, %{body: comment_body})
    assert comment_body =~ ~r/Pull request can be merged/

    assert successful_jobs(pr) == ~w(air_compile air_test cloak_compile cloak_test compliance prepare report_mergeable)
  end

  test "delayed PR approval", %{repo_data: repo_data} do
    pr = pr(approved?: false)
    repo_data = add_pr(repo_data, pr)
    AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)
    wait_for_jobs_to_finish(pr)

    assert successful_jobs(pr) == ~w(air_compile air_test cloak_compile cloak_test prepare)
    assert_pr_comment(pr, %{body: comment_body})
    assert comment_body =~ ~r/Standard tests have passed/

    {pr, _repo_data} = update_pr_data(repo_data, pr, &put_in(&1.approved?, true))
    wait_for_jobs_to_finish(pr)

    assert successful_job?(pr, "compliance")
    assert successful_job?(pr, "report_mergeable")
  end

  test "merge conflict", %{repo_data: repo_data} do
    pr = pr(merge_state: :conflicting)
    repo_data = add_pr(repo_data, pr)
    AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)
    wait_for_jobs_to_finish(pr)
    assert successful_jobs(pr) == ["prepare"]
  end

  test "resolved merge conflict", %{repo_data: repo_data} do
    pr = pr(merge_state: :conflicting)
    repo_data = add_pr(repo_data, pr)
    AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)
    wait_for_jobs_to_finish(pr)

    {pr, _repo_data} = update_pr_data(repo_data, pr, &%{&1 | merge_state: :mergeable, merge_sha: new_sha()})

    wait_for_jobs_to_finish(pr)

    assert successful_jobs(pr) == [
             "air_compile",
             "air_test",
             "cloak_compile",
             "cloak_test",
             "prepare"
           ]
  end

  test "unknown merge state", %{repo_data: repo_data} do
    pr = pr(merge_state: :unknown)
    repo_data = add_pr(repo_data, pr)
    AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)
    wait_for_jobs_to_finish(pr)
    assert successful_jobs(pr) == ["prepare"]
  end

  test "build restarted on new PR commit", %{repo_data: repo_data} do
    pr = pr(approved?: true)
    repo_data = add_pr(repo_data, pr)
    AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)
    wait_for_jobs_to_finish(pr)

    {pr, _} = update_pr_data(repo_data, pr, &%{&1 | sha: new_sha()})

    assert_posted_status(pr, "mergeable", %{
      state: :pending,
      description: "awaiting air_compile, air_test, cloak_compile, cloak_test"
    })

    assert_posted_status(pr, "mergeable", %{
      state: :success,
      description: "pull request can be merged"
    })
  end

  test "failing compilation", %{repo_data: repo_data} do
    pr = pr(approved?: true)
    repo_data = add_pr(repo_data, pr)

    ExUnit.CaptureLog.capture_log(fn ->
      fail_on_container_command(pr, "air", "mix compile")
      AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)
      wait_for_jobs_to_finish(pr)
    end)

    assert successful_jobs(pr) == ["cloak_compile", "cloak_test", "prepare"]
    assert failed_jobs(pr) == ["air_compile", "report_standard_tests"]

    assert_posted_status(pr, "mergeable", %{state: :error, description: "air_compile failed"})
    assert_pr_comment(pr, %{body: comment_body})
    assert comment_body =~ ~r/air_compile job errored/
  end

  test "failing test", %{repo_data: repo_data} do
    pr = pr(approved?: true)
    repo_data = add_pr(repo_data, pr)

    ExUnit.CaptureLog.capture_log(fn ->
      fail_on_container_command(pr, "air", "make test")
      AircloakCI.Build.PullRequest.ensure_started(pr, repo_data)
      wait_for_jobs_to_finish(pr)
    end)

    assert successful_jobs(pr) == ["air_compile", "cloak_compile", "cloak_test", "prepare"]
    assert failed_jobs(pr) == ["air_test", "report_standard_tests"]

    assert_posted_status(pr, "mergeable", %{state: :error, description: "air_test failed"})
    assert_pr_comment(pr, %{body: comment_body})
    assert comment_body =~ ~r/air_test job errored/
  end
end
