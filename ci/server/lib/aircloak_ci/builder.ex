defmodule AircloakCI.Builder do
  @moduledoc "CI builder engine."

  require Logger
  alias AircloakCI.{Build, Github}

  @opaque t :: %{current_jobs: [job], builds: %{}}
  @opaque job :: %{
    pid: pid,
    pr: Github.API.pull_request,
    type: module,
  }

  @aircloak_ci_name "continuous-integration/aircloak/ci"


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new builder instance."
  @spec new() :: t
  def new(), do:
    %{current_jobs: [], builds: %{}}

  @doc "Processes pending pull requests."
  @spec process_prs(t, Github.API.repo_data) :: t
  def process_prs(builder, repo_data) do
    builder = cancel_needless_builds(builder, repo_data.pull_requests)
    Enum.reduce(repo_data.pull_requests, builder, &maybe_start_job(&2, &1))
  end

  @doc "Force starts the build of the given pull request."
  @spec force_build(t, Github.API.pull_request) :: {:ok | {:error, String.t}, t}
  def force_build(builder, pr) do
    cond do
      running?(builder, pr) -> {{:error, "build for this PR is already running"}, builder}
      not pr.mergeable? or pr.merge_sha == nil -> {{:error, "this PR is not mergeable"}, builder}
      true ->
        builder = initialize_build(builder, pr)
        if ci_possible?(builder, pr) do
          {:ok, start_job(builder, pr)}
        else
          {{:error, "can't run CI for this PR"}, builder}
        end
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
        Logger.info("build for #{pr_log_display(job.pr)} finished with the result `#{result.outcome}`")
        report_status(job, build_status(result.outcome))
        {:ok, builder}
    end
  end
  def handle_message(builder, {:EXIT, pid, reason}) do
    case Enum.split_with(builder.current_jobs, &(&1.pid == pid)) do
      {[], _} -> :error
      {[job], remaining_jobs} ->
        if reason != :normal do
          report_status(job, :failure, reason)
          Logger.error("build for #{pr_log_display(job.pr)} crashed")
        end
        {:ok, %{builder | current_jobs: remaining_jobs}}
    end
  end
  def handle_message(_builder, _other), do:
    :error


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp maybe_start_job(builder, pr) do
    with \
      true <- startable?(builder, pr),
      builder = initialize_build(builder, pr),
      true <- ci_possible?(builder, pr)
    do
      start_job(builder, pr)
    else
      _ -> builder
    end
  end

  defp initialize_build(builder, pr) do
    with \
      :error <- Map.fetch(builder.builds, build_key(pr)),
      build = Build.for_pull_request(pr),
      :ok <- Build.initialize(build)
    do
      put_in(builder.builds[build_key(pr)], build)
    else
      _ -> builder
    end
  end

  defp build_key(pr), do: {pr.title, pr.merge_sha}

  defp start_job(builder, pr) do
    Logger.info("starting the build for #{pr_log_display(pr)}")

    build = build(builder, pr)
    job = %{pr: pr, build: build, pid: start_job_task(build)}
    report_status(job, :pending)

    update_in(builder.current_jobs, &[job | &1])
  end

  defp start_job_task(build) do
    me = self()
    {:ok, pid} = Task.start_link(fn ->
      send(me, {:job_result, %{pid: self(), outcome: AircloakCI.Compliance.run(build)}})
    end)
    pid
  end

  defp build(builder, pr), do:
    Map.fetch!(builder.builds, build_key(pr))

  defp startable?(builder, pr), do:
    not running?(builder, pr) and
    pr.mergeable? and
    pr.merge_sha != nil and
    pr.approved? and
    pr.status_checks["continuous-integration/travis-ci/pr"] == :success and
    pr.status_checks["continuous-integration/travis-ci/push"] == :success and
    pr.status_checks[@aircloak_ci_name] in [nil, :pending]

  defp ci_possible?(builder, pr), do:
    not is_nil(Build.ci_version(build(builder, pr)))

  defp running?(builder, pr), do:
    Enum.any?(builder.current_jobs, &(&1.pr.number == pr.number))

  defp cancel_needless_builds(builder, pending_prs) do
    remaining_builds = Map.take(builder.builds, Enum.map(pending_prs, &build_key/1))
    {remaining_jobs, outdated_jobs} = Enum.split_with(builder.current_jobs, &(valid_pr?(&1.pr, pending_prs)))
    Enum.each(outdated_jobs, &cancel_job/1)
    %{builder | current_jobs: remaining_jobs, builds: remaining_builds}
  end

  defp valid_pr?(pr, pending_prs), do:
    Enum.any?(pending_prs, &(&1.number == pr.number and &1.merge_sha == pr.merge_sha))

  defp cancel_job(%{pid: pid} = job) do
    Logger.info("cancelling outdated build for PR #{job.pr.number}")
    Process.exit(pid, :kill)
    receive do
      {:EXIT, ^pid, _reason} -> :ok
    end
  end

  defp report_status(job, state, context \\ nil) do
    Github.put_status_check_state!(job.pr.repo.owner, job.pr.repo.name, job.pr.sha, @aircloak_ci_name, state)
    maybe_send_comment(job, state, context)
  end

  defp maybe_send_comment(job, :success, _context), do:
    send_comment(job, "Compliance build succeeded 👍")
  defp maybe_send_comment(job, :error, _context), do:
    send_comment(job, Enum.join(["Compliance build errored 😞", "", "Log tail:", "```", log_tail(job), "```"], "\n"))
  defp maybe_send_comment(job, :failure, crash_reason), do:
    send_comment(job,
      Enum.join(
        [
          "Compliance build crashed 😞", "",
          "```", Exception.format_exit(crash_reason), "```", "",
          "Log tail:", "```", log_tail(job), "```"
        ],
        "\n"
      )
    )
  defp maybe_send_comment(_job, _, _other_status), do: :ok

  if Mix.env == :prod do
    defp send_comment(job, body), do:
      Github.post_comment(job.pr.repo.owner, job.pr.repo.name, job.pr.number, body)
  else
    defp send_comment(job, body) do
      IO.puts "PR comment for `#{inspect(job)}`:"
      IO.puts body
    end
  end

  defp build_status(:ok), do: :success
  defp build_status(:error), do: :error

  defp pr_log_display(pr), do:
    "PR `#{pr.title}` (##{pr.number})"

  defp log_tail(job) do
    max_lines = 100
    lines = job.build |> Build.log_contents() |> String.split("\n")

    lines
    |> Enum.drop(max(length(lines) - max_lines, 0))
    |> Enum.join("\n")
  end
end
