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
  def init(nil, state) do
    :timer.send_interval(:timer.seconds(5), self(), :report_mergeable)
    {:ok, report_mergeable(%{state | data: %{mergeable_info: nil}})}
  end

  @impl Build.Server
  def handle_source_change(state) do
    {:noreply, report_mergeable(state)}
  end

  @impl Build.Server
  def handle_job_succeeded(job_name, state), do: {:noreply, state |> start_next_job(job_name) |> report_mergeable()}

  @impl Build.Server
  def handle_job_failed(_job, _reason, state), do: {:noreply, report_mergeable(state)}

  @impl Build.Server
  def handle_info(:report_mergeable, state), do: {:noreply, report_mergeable(state)}
  def handle_info(other, state), do: super(other, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {Build.Registry, {:pull_request, pr.number}}}

  defp start_next_job(state, job_name) do
    if check_mergeable(state) == :ok do
      case Build.Server.job_type(job_name) do
        "prepare" -> Job.Compile.start(state)
        "compile" -> state |> Job.Test.start() |> maybe_start_compliance()
        "test" -> maybe_start_compliance(state)
        _other -> state
      end
    else
      state
    end
  end

  defp maybe_start_compliance(state) do
    if check_standard_tests(state) == :ok and check_approved(state) == :ok,
      do: Job.Compliance.start(state),
      else: state
  end

  defp report_mergeable(state) do
    {status, message} =
      with :ok <- check_mergeable(state),
           :ok <- check_standard_tests(state),
           :ok <- check_approved(state),
           :ok <- check_compliance(state),
        do: {:success, "pull request can be merged"}

    if status == :success and not LocalProject.finished?(state.project, "report_mergeable") do
      merge_message = "Pull request can be merged #{AircloakCI.Emoji.happy()}"
      Github.comment_on_issue(state.source.repo.owner, state.source.repo.name, state.source.number, merge_message)
      LocalProject.set_job_outcome(state.project, "report_mergeable", :ok)
    end

    new_info = {state.source.sha, message, status}

    if new_info != state.data.mergeable_info do
      Github.put_status_check_state(
        state.source.repo.owner,
        state.source.repo.name,
        state.source.sha,
        "continuous-integration/aircloak/mergeable",
        message,
        status
      )

      put_in(state.data.mergeable_info, new_info)
    else
      state
    end
  end

  defp check_mergeable(%{source: %{merge_state: :mergeable}}), do: :ok
  defp check_mergeable(%{source: %{merge_state: :unknown}}), do: {:pending, "awaiting merge status"}
  defp check_mergeable(%{source: %{merge_state: :conflicting}}), do: {:error, "there are merge conflicts"}

  defp check_approved(%{source: %{approved?: true}}), do: :ok
  defp check_approved(%{source: %{approved?: false}}), do: {:pending, "awaiting approval"}

  defp check_compliance(state) do
    case LocalProject.job_outcome(state.project, "compliance") do
      :ok -> :ok
      nil -> {:pending, "awaiting compliance"}
      _ -> {:error, "compliance test failed"}
    end
  end

  defp check_standard_tests(state) do
    job_outcomes = LocalProject.job_outcomes(state.project)

    statuses =
      state.project
      |> LocalProject.components()
      |> Enum.flat_map(&["#{&1}_compile", "#{&1}_test"])
      |> Enum.map(&{&1, Map.get(job_outcomes, &1, :pending)})

    with :ok <- check_failures(statuses), do: check_pending(statuses)
  end

  defp check_failures(statuses) do
    statuses
    |> Enum.reject(fn({_, status}) -> status in [:ok, :pending] end)
    |> Enum.map(fn({name, _}) -> name end)
    |> case do
        [] -> :ok
        failed_jobs -> {:error, "#{Enum.join(failed_jobs, ", ")} failed"}
      end
  end

  defp check_pending(statuses) do
    statuses
    |> Enum.filter(&match?({_, :pending}, &1))
    |> Enum.map(fn({name, _}) -> name end)
    |> case do
        [] -> :ok
        pending_jobs -> {:pending, "awaiting #{Enum.join(pending_jobs, ", ")}"}
      end
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data), do:
    Build.Server.start_link(__MODULE__, :pull_request, pr.number, repo_data, nil, name: name(pr))
end
