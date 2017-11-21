defmodule AircloakCI.Builder do
  @moduledoc "CI builder engine."

  require Logger
  alias AircloakCI.Github

  @opaque t :: %{current_jobs: [job]}
  @opaque job :: %{
    pid: pid,
    pr: Github.pull_request,
    type: module
  }

  @aircloak_ci_name "continuous-integration/aircloak/ci"


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates the new builder instance."
  @spec new() :: t
  def new(), do:
    %{current_jobs: []}

  @doc "Processes the provided pull request."
  @spec process_pr(t, Github.pull_request) :: t
  def process_pr(builder, pr), do:
    builder
    |> cancel_outdated(pr)
    |> maybe_start_job(pr)

  @doc "Force starts the build of the given pull request."
  @spec force_build(t, Github.pull_request) :: :ok | {:error, String.t}
  def force_build(builder, pr) do
    cond do
      running?(builder, pr) -> {:error, "build for this PR is already running"}
      not pr.mergeable? or pr.merge_sha == nil -> {:error, "this PR is not mergeable"}
      true -> {:ok, start_job(builder, pr)}
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
        Logger.info("build for PR #{job.pr.number} finished with the result `#{result.outcome}`")
        report_status(job.pr, build_status(result.outcome))
        {:ok, builder}
    end
  end
  def handle_message(builder, {:EXIT, pid, reason}) do
    case Enum.split_with(builder.current_jobs, &(&1.pid == pid)) do
      {[], _} -> :error
      {[job], remaining_jobs} ->
        if reason != :normal, do: report_status(job.pr, :failure)
        {:ok, %{builder | current_jobs: remaining_jobs}}
    end
  end
  def handle_message(_builder, _other), do:
    :error



  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp maybe_start_job(builder, pr) do
    if can_start_job?(builder, pr) do
      start_job(builder, pr)
    else
      builder
    end
  end

  defp start_job(builder, pr) do
    report_status(pr, :pending)

    me = self()
    {:ok, pid} = Task.start_link(fn ->
      send(me, {:job_result, %{pid: self(), outcome: AircloakCI.Compliance.run(pr)}})
    end)

    update_in(builder.current_jobs, &[%{pid: pid, pr: pr} | &1])
  end

  defp can_start_job?(builder, pr), do:
    not running?(builder, pr) and
    pr.mergeable? and
    pr.merge_sha != nil and
    pr.approved? and
    pr.status_checks["continuous-integration/travis-ci/pr"] == :success and
    pr.status_checks["continuous-integration/travis-ci/push"] == :success and
    pr.status_checks[@aircloak_ci_name] in [nil, :pending] and
    # TODO: remove this temp filter
    pr.target_branch == "sasa/extract-compliance-ci"

  defp running?(builder, pr), do:
    Enum.any?(builder.current_jobs, &(&1.pr.number == pr.number))

  defp cancel_outdated(builder, pr) do
    {outdated, remaining} =
      Enum.split_with(
        builder.current_jobs,
        &(&1.pr.number == pr.number and &1.pr.merge_sha != pr.merge_sha)
      )
    Enum.each(outdated, &cancel_job/1)
    %{builder | current_jobs: remaining}
  end

  defp cancel_job(%{pid: pid} = job) do
    Logger.info("cancelling outdated build for PR #{job.pr.number}")
    Process.exit(pid, :kill)
    receive do
      {:EXIT, ^pid, _reason} -> :ok
    end
  end

  defp report_status(pr, state), do:
    Github.RateLimiter.put_status_check_state!(pr.repo.owner, pr.repo.name, pr.sha, @aircloak_ci_name, state)

  defp build_status(:ok), do: :success
  defp build_status(:error), do: :error
end
