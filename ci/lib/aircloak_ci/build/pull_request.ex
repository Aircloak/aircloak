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
    {:ok, start_preparation_job(state)}

  @impl JobRunner
  def handle_restart(state), do:
    {:noreply, start_preparation_job(state)}

  @impl JobRunner
  def handle_source_change(state), do:
    {:noreply, maybe_start_ci(state)}

  @impl JobRunner
  def handle_job_succeeded(Task.Prepare, state), do: {:noreply, maybe_compile_project(state)}
  def handle_job_succeeded(Task.Compile, state), do: {:noreply, maybe_start_ci(state)}
  def handle_job_succeeded(Task.Compliance, state), do: {:noreply, state}

  @impl JobRunner
  def handle_job_failed(Task.Prepare, _reason, state) do
    LocalProject.clean(state.project)
    {:noreply, start_preparation_job(state, delay: :timer.seconds(10))}
  end
  def handle_job_failed(Task.Compliance, crash_reason, state), do:
    {:stop, :normal, Task.Compliance.handle_finish(state, :failure, crash_reason)}
  def handle_job_failed(other_job, reason, state), do:
    super(other_job, reason, state)

  @impl JobRunner
  def handle_call(:force_build, _from, state) do
    state = JobRunner.terminate_all_jobs(state)
    LocalProject.mark_forced(state.project)
    {:reply, :ok, start_preparation_job(state)}
  end

  @impl JobRunner
  def handle_info({Task.Compliance, result}, state), do:
    {:stop, :normal, Task.Compliance.handle_finish(state, result, nil)}
  def handle_info(other, state), do:
    super(other, state)


  # -------------------------------------------------------------------
  # Project preparation
  # -------------------------------------------------------------------

  defp start_preparation_job(state, opts \\ []), do:
    Task.Prepare.run(state, base_branch(state), opts)

  defp base_branch(state), do: Enum.find(state.repo_data.branches, &(&1.name == state.source.target_branch))

  defp maybe_compile_project(state) do
    if LocalProject.ci_possible?(state.project), do: Task.Compile.run(state), else: state
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {Build.Registry, {:pull_request, pr.number}}}

  defp maybe_start_ci(state) do
    if Enum.any?([Task.Prepare, Task.Compile], &JobRunner.running?(state, &1)) do
      state
    else
      maybe_start_compliance(state)
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
