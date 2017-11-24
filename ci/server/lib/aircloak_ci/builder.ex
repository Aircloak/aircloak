defmodule AircloakCI.Builder do
  @moduledoc "CI builder engine."

  require Logger
  alias AircloakCI.{Build, Github}

  @opaque t :: %{current_jobs: [job]}
  @opaque job :: %{
    name: String.t,
    type: :compliance | :branch,
    build: Build.t,
    pid: pid,
    start: integer(),
    pr: Github.API.pull_request | nil,
    branch: Github.API.branch
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new builder instance."
  @spec new() :: t
  def new(), do:
    %{current_jobs: []}

  @doc "Processes pending pull requests."
  @spec process_repo_data(t, Github.API.repo_data) :: t
  def process_repo_data(builder, repo_data), do:
    builder
    |> cancel_needless_builds(repo_data.pull_requests)
    |> process_base_branches(repo_data)
    |> process_pull_requests(repo_data)

  @doc "Force starts the build of the given pull request."
  @spec force_build(t, Github.API.pull_request) :: {:ok, t} | {:error, String.t}
  def force_build(builder, pr) do
    case check_pr_start_preconditions(builder, pr) do
      :ok ->
        pr |> Build.for_pull_request() |> Build.set_status(:force_start)
        {:ok, process_pr(builder, pr)}
      {:error, _} = error -> error
    end
  end

  @doc "Handles a builder specific message."
  @spec handle_message(t, any) :: {:ok, t} | :error
  def handle_message(builder, {:job_result, result}) do
    case Enum.find(builder.current_jobs, &(&1.pid == result.pid)) do
      nil ->
        # job doesn't exist, probably because it was cancelled just before it finished
        {:ok, builder}

      job ->
        Logger.info("build `#{job.name}` finished with the result `#{result.outcome}`")
        handle_job_finish(job, build_status(result.outcome), nil)
        {:ok, builder}
    end
  end
  def handle_message(builder, {:EXIT, pid, reason}) do
    case Enum.split_with(builder.current_jobs, &(&1.pid == pid)) do
      {[], _} -> :error
      {[job], remaining_jobs} ->
        if reason != :normal do
          Logger.error("build `#{job.name}` crashed")
          handle_job_finish(job, :failure, reason)
        end
        {:ok, %{builder | current_jobs: remaining_jobs}}
    end
  end
  def handle_message(_builder, _other), do:
    :error


  # -------------------------------------------------------------------
  # Branch builds
  # -------------------------------------------------------------------

  defp process_base_branches(builder, repo_data) do
    required_branches = repo_data.pull_requests |> Enum.map(&(&1.target_branch)) |> Enum.uniq()

    repo_data.branches
    |> Enum.reject(&(not &1.name in required_branches))
    |> Enum.reduce(builder, &process_branch(&2, &1))
  end

  defp process_branch(builder, branch) do
    build = Build.for_branch(branch.repo, branch.name, branch.sha)
    if build_possible?(build) and Build.status(build) != :finished and Enum.empty?(builder.current_jobs) do
      start_branch_job(builder, build, branch)
    else
      builder
    end
  end

  defp start_branch_job(builder, build, branch) do
    job_name = "branch #{branch.name}"
    job = %{new_job(job_name, :branch, build, start_task(fn -> Build.compile(build) end)) | branch: branch}
    update_in(builder.current_jobs, &[job | &1])
  end


  # -------------------------------------------------------------------
  # PR builds
  # -------------------------------------------------------------------

  defp process_pull_requests(builder, repo_data), do:
    Enum.reduce(repo_data.pull_requests, builder, &process_pr(&2, &1))

  defp process_pr(builder, pr) do
    case check_pr_start_preconditions(builder, pr) do
      :ok ->
        if pr_build_status(pr) != :finished or force_start?(pr) do
          maybe_start_pr_job(builder, pr)
        else
          builder
        end

      {:error, _} -> builder
    end
  end

  defp maybe_start_pr_job(builder, pr) do
    with \
      {_status, true} <- {"waiting for Travis builds to succeed", travis_succeeded?(pr) or force_start?(pr)},
      {_status, true} <- {"waiting for approval", pr.approved? or force_start?(pr)},
      {_status, true} <- {"waiting in queue", Enum.empty?(builder.current_jobs)}
    do
      start_pr_job(builder, pr)
    else
      {status, false} ->
        send_status_to_github(pr, :pending, status)
        builder
    end
  end

  defp travis_succeeded?(pr), do:
    (pr.status_checks["continuous-integration/travis-ci/pr"] || %{status: nil}).status == :success and
    (pr.status_checks["continuous-integration/travis-ci/push"] || %{status: nil}).status == :success

  defp start_pr_job(builder, pr) do
    send_status_to_github(pr, :pending, "build started")

    build = Build.for_pull_request(pr)
    job_name = ~s/PR "#{pr.title}" (##{pr.number})/
    job = %{new_job(job_name, :compliance, build, start_task(fn -> AircloakCI.Compliance.run(build) end)) | pr: pr}
    update_in(builder.current_jobs, &[job | &1])
  end

  defp check_pr_start_preconditions(builder, pr) do
    with \
      {_error, true} <- {"already running", not pr_running?(builder, pr)},
      {_error, true} <- {"unmergeable", pr.mergeable? and pr.merge_sha != nil},
      {_error, true} <- {"CI not possible", build_possible?(Build.for_pull_request(pr))}
    do
      :ok
    else
      {error, false} -> {:error, error}
    end
  end

  defp pr_running?(builder, pr), do:
    Enum.any?(builder.current_jobs, &(&1.pr != nil && &1.pr.number == pr.number))

  defp pr_build_status(pr), do:
    pr |> Build.for_pull_request() |> Build.status()

  defp force_start?(pr), do:
    pr_build_status(pr) == :force_start

  defp cancel_needless_builds(builder, pending_prs) do
    {remaining_jobs, outdated_jobs} = Enum.split_with(builder.current_jobs, &valid_pr?(&1.pr, pending_prs))
    Enum.each(outdated_jobs, &cancel_job/1)
    %{builder | current_jobs: remaining_jobs}
  end

  defp valid_pr?(nil, _pending_prs), do: true
  defp valid_pr?(pr, pending_prs), do:
    Enum.any?(pending_prs, &(&1.number == pr.number and &1.merge_sha == pr.merge_sha))

  defp send_status_to_github(pr, status, description) do
    status_context = "continuous-integration/aircloak/ci"
    current_description = (pr.status_checks[status_context] || %{description: nil}).description
    if description != current_description, do:
      Github.put_status_check_state(pr.repo.owner, pr.repo.name, pr.sha, status_context, description, status)
  end

  defp send_comment(%{type: :compliance} = job, body), do:
    Github.post_comment(job.pr.repo.owner, job.pr.repo.name, job.pr.number, body)

  defp description(:success), do: "build succeeded"
  defp description(:error), do: "build errored"
  defp description(:failure), do: "build failed"

  defp comment(:success, _job, nil), do:
    "Compliance build succeeded 👍"
  defp comment(:error, job, nil), do:
    Enum.join(["Compliance build errored 😞", "", "Log tail:", "```", log_tail(job), "```"], "\n")
  defp comment(:failure, job, crash_reason), do:
    Enum.join(
      [
        "Compliance build crashed 😞", "",
        "```", Exception.format_exit(crash_reason), "```", "",
        "Log tail:", "```", log_tail(job), "```"
      ],
      "\n"
    )


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp new_job(name, type, build, pid) do
    Logger.info("starting the job `#{name}`")
    %{name: name, type: type, build: build, pid: pid, pr: nil, branch: nil, start: :erlang.monotonic_time(:second)}
  end

  defp start_task(lambda) do
    me = self()
    {:ok, pid} = Task.start_link(fn ->
      outcome = lambda.()
      send(me, {:job_result, %{pid: self(), outcome: outcome}})
    end)
    pid
  end

  defp build_possible?(build), do:
    Build.initialize(build) == :ok and not is_nil(Build.ci_version(build))

  defp cancel_job(%{pid: pid} = job) do
    Logger.info("cancelling outdated build `#{job.name}`")
    Process.exit(pid, :kill)
    receive do
      {:EXIT, ^pid, _reason} -> :ok
    end
  end

  defp handle_job_finish(job, status, context) do
    diff_sec = :erlang.monotonic_time(:second) - job.start
    time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

    Logger.info("job `#{job.name}` finished in #{time_output} min")
    Build.log(job.build, "finished with status `#{status}` in #{time_output} min")

    handle_finish(job, status, context)

    Build.set_status(job.build, :finished)
  end

  defp build_status(:ok), do: :success
  defp build_status(:error), do: :error

  defp handle_finish(%{type: :compliance} = job, status, context) do
    send_status_to_github(job.pr, status, description(status))
    send_comment(job, comment(status, job, context))
  end
  defp handle_finish(_other, _status, _context), do: :ok

  defp log_tail(job) do
    max_lines = 100
    lines = job.build |> Build.log_contents() |> String.split("\n")

    lines
    |> Enum.drop(max(length(lines) - max_lines, 0))
    |> Enum.join("\n")
  end
end
