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
  @spec ensure_started(Github.API.pull_request(), Github.API.repo_data()) :: pid
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
      pr = if Application.get_env(:aircloak_ci, :simulate_approval) == true, do: %{pr | approved?: true}, else: pr

      %{
        source: pr,
        base_branch: Enum.find(repo_data.branches, &(&1.name == pr.target_branch)),
        project: LocalProject.for_pull_request(pr)
      }
    end
  end

  @impl Build.Server
  def init(nil, state) do
    :timer.send_interval(:timer.seconds(5), self(), :report_status)
    {:ok, report_status(%{state | data: %{mergeable_info: nil}})}
  end

  @impl Build.Server
  def handle_source_change(state) do
    {:noreply, state |> start_next_jobs() |> report_status()}
  end

  @impl Build.Server
  def handle_job_succeeded(_job_name, state), do: {:noreply, state |> start_next_jobs() |> report_status()}

  @impl Build.Server
  def handle_job_failed(_job, _reason, state), do: {:noreply, report_status(state)}

  @impl Build.Server
  def handle_info(:report_status, state), do: {:noreply, report_status(state)}
  def handle_info(other, state), do: super(other, state)

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do: {:via, Registry, {Build.Registry, {:pull_request, pr.number}}}

  defp start_next_jobs(state) do
    if state.prepared? and check_mergeable(state) == :ok do
      state
      |> Job.Compile.start_if_possible()
      |> Job.Test.start_if_possible()
      |> maybe_start_compliance()
      |> maybe_start_system_test()
    else
      state
    end
  end

  defp maybe_start_compliance(state) do
    if check_standard_tests(state) == :ok and check_approved(state) == :ok and
         LocalProject.run_compliance?(state.project),
       do: Job.Compliance.start_if_possible(state),
       else: state
  end

  defp maybe_start_system_test(state) do
    if check_standard_tests(state) == :ok and check_approved(state) == :ok and
         LocalProject.run_system_test?(state.project),
       do: Job.SystemTest.start_if_possible(state),
       else: state
  end

  defp report_status(state) do
    if state.prepared?, do: report_standard_tests(state)
    report_mergeable(state)
  end

  defp report_standard_tests(state) do
    {status, _reason} =
      with :ok <- check_mergeable(state),
           :ok <- check_standard_tests(state),
           do: {:success, nil}

    if LocalProject.job_outcome(state.project, "report_standard_tests") != status do
      if status == :success and not state.source.approved? do
        Github.comment_on_issue(
          state.source.repo.owner,
          state.source.repo.name,
          state.source.number,
          "Standard tests have passed #{AircloakCI.Emoji.happy()}"
        )
      end

      LocalProject.set_job_outcome(state.project, "report_standard_tests", status)
    end
  end

  defp report_mergeable(state) do
    {status, message} =
      with :ok <- check_mergeable(state),
           :ok <- check_standard_tests(state),
           :ok <- check_final_tests(state),
           do: {:success, "pull request can be merged"}

    if status == :success and not LocalProject.finished?(state.project, "report_mergeable") do
      Github.comment_on_issue(
        state.source.repo.owner,
        state.source.repo.name,
        state.source.number,
        "Pull request can be merged #{AircloakCI.Emoji.happy()}"
      )

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

  defp check_final_tests(state) do
    final_jobs = %{
      "approval" => approval_outcome(state),
      "compliance" => compliance_outcome(state),
      "system_test" => system_test_outcome(state)
    }

    with :ok <- check_failures(final_jobs), do: check_pending(final_jobs)
  end

  defp approval_outcome(state), do: if(state.source.approved?, do: :ok, else: :pending)

  defp compliance_outcome(state) do
    if LocalProject.run_compliance?(state.project),
      do: LocalProject.job_outcome(state.project, "compliance") || :pending,
      else: :ok
  end

  defp system_test_outcome(state) do
    if LocalProject.run_system_test?(state.project),
      do: LocalProject.job_outcome(state.project, "system_test") || :pending,
      else: :ok
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
    |> Enum.reject(fn {_, status} -> status in [:ok, :pending] end)
    |> Enum.map(fn {name, _} -> name end)
    |> case do
      [] -> :ok
      failed_jobs -> {:error, "#{Enum.join(failed_jobs, ", ")} failed"}
    end
  end

  defp check_pending(statuses) do
    statuses
    |> Enum.filter(&match?({_, :pending}, &1))
    |> Enum.map(fn {name, _} -> name end)
    |> case do
      [] -> :ok
      pending_jobs -> {:pending, "awaiting #{Enum.join(pending_jobs, ", ")}"}
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data),
    do:
      Build.Server.start_link(
        __MODULE__,
        :pull_request,
        pr.number,
        repo_data,
        nil,
        name: name(pr)
      )
end
