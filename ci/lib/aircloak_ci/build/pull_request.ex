defmodule AircloakCI.Build.PullRequest do
  @moduledoc """
  This module powers the process responsible for the entire build of the single PR.

  The process will start various child jobs to initialize the repo and run different tests.
  """

  use AircloakCI.Build.Server, restart: :temporary
  require Logger
  alias AircloakCI.{Build, Github, LocalProject}
  alias AircloakCI.Build.Job


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given pull request is started."
  @spec ensure_started(Github.API.pull_request, Github.API.repo_data) :: pid
  def ensure_started(pr, repo_data) do
    case Build.Supervisor.start_build(__MODULE__, [pr, repo_data]) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end


  # -------------------------------------------------------------------
  # Build.Server callbacks
  # -------------------------------------------------------------------

  @impl Build.Server
  def build_source(pr_number, repo_data) do
    pr = Enum.find(repo_data.pull_requests, &(&1.number == pr_number))
    if is_nil(pr) do
      nil
    else
      %{
        source: pr,
        base_branch: Enum.find(repo_data.branches, &(&1.name == pr.target_branch)),
        project: LocalProject.for_pull_request(pr)
      }
    end
  end

  @impl Build.Server
  def init(nil, state), do:
    {:ok, state}

  @impl Build.Server
  def handle_source_change(state) do
    state = maybe_start_ci(state)
    maybe_report_mergeable(state)
    {:noreply, state}
  end

  @impl Build.Server
  def handle_job_succeeded("compile", state), do: {:noreply, maybe_start_ci(state)}
  def handle_job_succeeded(other, state), do: super(other, state)

  @impl Build.Server
  # Note: we'll start the CI even if the compilation failed. If the resulting errors occur again, they will be properly
  # reported to the author.
  def handle_job_failed("compile", _reason, state), do: {:noreply, maybe_start_ci(state)}
  def handle_job_failed(other, reason, state), do: super(other, reason, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {Build.Registry, {:pull_request, pr.number}}}

  defp maybe_start_ci(%{compiled?: false} = state), do: state
  defp maybe_start_ci(%{compiled?: true} = state), do:
    state
    |> Job.Compliance.run()
    |> Job.Test.run()

  defp maybe_report_mergeable(state) do
    if \
      state.compiled? and
      state.source.mergeable? and
      state.source.approved? and
      ci_checks_succeeded?(state) and
      Enum.empty?(Build.Server.running_jobs(state)) and
      not LocalProject.finished?(state.project, "report_mergeable"),
      do: report_mergeable(state)
  end

  defp report_mergeable(state) do
    Github.comment_on_issue(state.source.repo.owner, state.source.repo.name, state.source.number, merge_message())
    LocalProject.mark_finished(state.project, "report_mergeable")
  end

  defp merge_message(), do:
    "Pull request can be merged #{AircloakCI.Emoji.happy()}"

  defp ci_checks_succeeded?(state), do:
    required_ci_checks()
    |> Stream.map(&state.source.status_checks[&1][:status])
    |> Enum.all?(&(&1 == :success))

  defp required_ci_checks(), do:
    [
      "continuous-integration/travis-ci/pr",
      "continuous-integration/aircloak/compliance",
      "continuous-integration/aircloak/cloak_test",
    ]


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data), do:
    Build.Server.start_link(__MODULE__, :pull_request, pr.number, repo_data, nil, name: name(pr))
end
