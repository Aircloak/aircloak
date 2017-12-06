defmodule AircloakCI.Build.PullRequest do
  @moduledoc """
  This module powers the process responsible for the entire build of the single PR.

  The process will start various child jobs to initialize the repo and run different tests.
  """

  use AircloakCI.JobRunner, restart: :temporary
  require Logger
  alias AircloakCI.{Build, Github, JobRunner, LocalProject}
  alias AircloakCI.Build.Task


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given pull request is started."
  @spec ensure_started(Github.API.pull_request, Github.API.repo_data) :: :ok
  def ensure_started(pr, repo_data) do
    case Build.Supervisor.start_build(__MODULE__, [pr, repo_data]) do
      {:ok, _} -> :ok
      {:error, {:already_started, _pid}} -> :ok
    end
  end

  @doc "Force starts the build of the given pull request."
  @spec force_build(Github.API.pull_request, Github.API.repo_data) :: :ok
  def force_build(pr, repo_data) do
    ensure_started(pr, repo_data)
    GenServer.call(name(pr), :force_build, :timer.minutes(30))
  end


  # -------------------------------------------------------------------
  # JobRunner callbacks
  # -------------------------------------------------------------------

  @impl AircloakCI.JobRunner
  def create_project(state), do:
    LocalProject.for_pull_request(state.source)

  @impl AircloakCI.JobRunner
  def refresh_source(state), do:
    Enum.find(state.repo_data.pull_requests, &(&1.number == state.source.number))

  @impl JobRunner
  def init(nil, state), do:
    {:ok, prepare_project(%{state | data: %{prepared?: false}})}

  @impl JobRunner
  def handle_restart(state), do:
    {:noreply, prepare_project(state)}

  @impl JobRunner
  def handle_source_change(state), do:
    {:noreply, maybe_start_ci(state)}

  @impl JobRunner
  def handle_job_succeeded(:project_preparation, state), do:
    {:noreply, maybe_start_ci(put_in(state.data.prepared?, true))}
  def handle_job_succeeded(:compliance, state), do:
    {:noreply, state}
  def handle_job_succeeded(Task.Compliance, state), do:
    {:noreply, state}

  @impl JobRunner
  def handle_job_failed(Task.Compliance, crash_reason, state), do:
    {:stop, :normal, Task.Compliance.handle_finish(state, :failure, crash_reason)}
  def handle_job_failed(other_job, reason, state), do:
    super(other_job, reason, state)

  @impl JobRunner
  def handle_call(:force_build, _from, state) do
    state = JobRunner.terminate_all_jobs(state)
    LocalProject.mark_forced(state.project)
    {:reply, :ok, prepare_project(state)}
  end

  @impl JobRunner
  def handle_info({Task.Compliance, result}, state), do:
    {:stop, :normal, Task.Compliance.handle_finish(state, result, nil)}
  def handle_info(other, state), do:
    super(other, state)


  # -------------------------------------------------------------------
  # Project preparation
  # -------------------------------------------------------------------

  defp prepare_project(%{project: project} = state) do
    target_branch = branch!(state.repo_data, state.source.target_branch)
    JobRunner.start_task(
      put_in(state.data.prepared?, :false),
      :project_preparation,
      fn -> init_project(project, target_branch) end
    )
  end

  defp branch!(repo_data, branch_name), do:
    %{} = Enum.find(repo_data.branches, &(&1.name == branch_name))

  defp init_project(project, target_branch) do
    unless LocalProject.initialized?(project), do:
      Build.Branch.transfer_project(target_branch, project)

    with :ok <- LocalProject.update_code(project) do
      if LocalProject.ci_possible?(project), do: Task.Compile.run(project)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {Build.Registry, {:pull_request, pr.number}}}

  defp maybe_start_ci(%{data: %{prepared?: false}} = state), do:
    state
  defp maybe_start_ci(%{data: %{prepared?: true}} = state) do
    if LocalProject.ci_possible?(state.project) do
      maybe_start_compliance(state)
    else
      LocalProject.log(state.project, "main", "can't run CI on this PR")
      state
    end
  end

  defp maybe_start_compliance(state) do
    if JobRunner.running?(state, Task.Compliance) do
      state
    else
      Task.Compliance.run(state)
    end
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data), do:
    JobRunner.start_link(__MODULE__, pr, repo_data, nil, name: name(pr))
end
