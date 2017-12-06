defmodule AircloakCI.Build.PullRequest do
  @moduledoc """
  This module powers the process responsible for the entire build of the single PR.

  The process will start various child jobs to initialize the repo and run different tests.
  """

  use AircloakCI.JobRunner.PullRequest, restart: :temporary
  require Logger
  alias AircloakCI.{Github, JobRunner, LocalProject}
  alias AircloakCI.Job.Compliance


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given pull request is started."
  @spec ensure_started(Github.API.pull_request, Github.API.repo_data) :: :ok
  def ensure_started(pr, repo_data) do
    case AircloakCI.Build.Supervisor.start_build(__MODULE__, [pr, repo_data]) do
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

  @impl JobRunner
  def init(nil, state), do:
    {:ok, prepare_project(state)}

  @impl JobRunner
  def handle_job_succeeded(:project_preparation, state), do:
    {:noreply, maybe_start_ci(state)}
  def handle_job_succeeded(:compliance, state), do:
    {:noreply, state}

  @impl JobRunner
  def handle_restart(state), do:
    {:noreply, prepare_project(state)}

  @impl JobRunner
  def handle_call(:force_build, _from, state) do
    state = JobRunner.terminate_all_jobs(state)
    LocalProject.mark_forced(state.project)
    {:reply, :ok, prepare_project(state)}
  end


  # -------------------------------------------------------------------
  # Project preparation
  # -------------------------------------------------------------------

  defp prepare_project(%{project: project} = state) do
    target_branch = branch!(state.repo_data, state.source.target_branch)
    JobRunner.start_job(
      state,
      :project_preparation,
      fn -> Task.start_link(fn -> init_project(project, target_branch) end) end
    )
  end

  defp branch!(repo_data, branch_name), do:
    %{} = Enum.find(repo_data.branches, &(&1.name == branch_name))

  defp init_project(project, target_branch) do
    unless LocalProject.initialized?(project), do:
      AircloakCI.Build.Branch.transfer_project(target_branch, project)

    with :ok <- LocalProject.update_code(project) do
      if LocalProject.ci_possible?(project), do: AircloakCI.Build.Task.Compile.run(project)
    end
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {AircloakCI.Build.Registry, {:pull_request, pr.number}}}

  defp maybe_start_ci(state) do
    if LocalProject.ci_possible?(state.project) do
      JobRunner.start_job(
        state,
        :compliance,
        fn -> Compliance.start_link(state.source, state.repo_data) end
      )
    else
      LocalProject.log(state.project, "main", "can't run CI on this PR")
      state
    end
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(pr, repo_data), do:
    JobRunner.start_link(__MODULE__, pr, repo_data, nil, name: name(pr))
end
