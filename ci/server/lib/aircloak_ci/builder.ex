defmodule AircloakCI.Builder do
  @moduledoc "CI builder engine."

  require Logger
  alias AircloakCI.{Build, Github}

  @opaque t :: %{current_jobs: [job]}
  @opaque job :: %{
    pr: Github.API.pull_request,
    build: Build.t,
    pid: pid,
    start: integer()
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new builder instance."
  @spec new() :: t
  def new(), do:
    %{current_jobs: []}

  @doc "Processes pending pull requests."
  @spec process_prs(t, Github.API.repo_data) :: t
  def process_prs(builder, repo_data) do
    builder = cancel_needless_builds(builder, repo_data.pull_requests)
    Enum.reduce(repo_data.pull_requests, builder, &maybe_start_job(&2, &1))
  end

  @doc "Force starts the build of the given pull request."
  @spec force_build(t, Github.API.pull_request) :: {:ok, t} | {:error, String.t}
  def force_build(builder, pr) do
    case check_start_preconditions(builder, pr) do
      :ok ->
        pr |> Build.for_pull_request() |> Build.set_status(:force_start)
        {:ok, maybe_start_job(builder, pr)}
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
          Logger.error("build for #{pr_log_display(job.pr)} crashed")
          report_status(job, :failure, reason)
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
    if startable?(builder, pr) do
      start_job(builder, pr)
    else
      builder
    end
  end

  defp start_job(builder, pr) do
    Logger.info("starting the build for #{pr_log_display(pr)}")

    build = Build.for_pull_request(pr)
    job = %{pr: pr, build: build, pid: start_job_task(build), start: :erlang.monotonic_time(:second)}
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

  defp check_start_preconditions(builder, pr) do
    with \
      {_error, true} <- {"already running", not running?(builder, pr)},
      {_error, true} <- {"unmergeable", pr.mergeable? and pr.merge_sha != nil},
      {_error, true} <- {"CI not possible", ci_possible?(pr)}
    do
      :ok
    else
      {error, false} -> {:error, error}
    end
  end

  defp startable?(builder, pr), do:
    check_start_preconditions(builder, pr) == :ok and (
      pr_build_status(pr) == :force_start or (
        pr.approved? and
        pr.status_checks["continuous-integration/travis-ci/pr"] == :success and
        pr.status_checks["continuous-integration/travis-ci/push"] == :success and
        pr_build_status(pr) != :finished
      )
    )

  defp running?(builder, pr), do:
    Enum.any?(builder.current_jobs, &(&1.pr.number == pr.number))

  defp ci_possible?(pr) do
    build = Build.for_pull_request(pr)
    Build.initialize(build) == :ok and not is_nil(Build.ci_version(build))
  end

  defp pr_build_status(pr), do:
    pr |> Build.for_pull_request() |> Build.status()

  defp cancel_needless_builds(builder, pending_prs) do
    {remaining_jobs, outdated_jobs} = Enum.split_with(builder.current_jobs, &(valid_pr?(&1.pr, pending_prs)))
    Enum.each(outdated_jobs, &cancel_job/1)
    %{builder | current_jobs: remaining_jobs}
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
    Github.put_status_check_state!(job.pr.repo.owner, job.pr.repo.name, job.pr.sha,
      "continuous-integration/aircloak/ci", state)
    handle_job_finish(job, state, context)
  end

  defp handle_job_finish(_job, :pending, _context), do: :ok
  defp handle_job_finish(job, status, context) do
    Build.set_status(job.build, :finished)
    diff_sec = :erlang.monotonic_time(:second) - job.start
    time_output = :io_lib.format("~b:~2..0b", [div(diff_sec, 60), rem(diff_sec, 60)])

    Logger.info("job #{pr_log_display(job.pr)} finished in #{time_output} min")
    Build.log(job.build, "finished with status `#{status}` in #{time_output} min")
    send_comment(job, comment(status, job, context))
  end

  defp comment(:success, _job, _context), do:
    "Compliance build succeeded 👍"
  defp comment(:error, job, _context), do:
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
