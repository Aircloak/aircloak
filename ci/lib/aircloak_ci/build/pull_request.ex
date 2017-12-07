defmodule AircloakCI.Build.PullRequest do
  @moduledoc """
  This module powers the process responsible for the entire build of the single PR.

  The process will start various child jobs to initialize the repo and run different tests.
  """

  use AircloakCI.Build.Server, restart: :temporary
  require Logger
  alias AircloakCI.{Build, Github, LocalProject}
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
  # Build.Server callbacks
  # -------------------------------------------------------------------

  @impl Build.Server
  def base_branch(state), do: Enum.find(state.repo_data.branches, &(&1.name == state.source.target_branch))

  @impl Build.Server
  def create_project(state), do:
    LocalProject.for_pull_request(state.source)

  @impl Build.Server
  def refresh_source(state), do:
    Enum.find(state.repo_data.pull_requests, &(&1.number == state.source.number))

  @impl Build.Server
  def init(nil, state), do:
    {:ok, state}

  @impl Build.Server
  def handle_source_change(state), do:
    {:noreply, maybe_start_ci(state)}

  @impl Build.Server
  def handle_job_succeeded(Task.Compile, state), do: {:noreply, maybe_start_ci(state)}
  def handle_job_succeeded(Task.Compliance, state), do: {:noreply, state}

  @impl Build.Server
  def handle_call(:force_build, _from, state), do:
    {:reply, :ok, Build.Server.restart(state, before_start: &LocalProject.mark_forced(&1.project))}

  @impl Build.Server
  def handle_info({Task.Compliance, result}, state), do:
    {:stop, :normal, Task.Compliance.handle_finish(state, result, nil)}
  def handle_info(other, state), do:
    super(other, state)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {Build.Registry, {:pull_request, pr.number}}}

  defp maybe_start_ci(state) do
    if Enum.any?([Task.Prepare, Task.Compile], &Build.Server.running?(state, &1)) do
      state
    else
      maybe_start_compliance(state)
    end
  end

  defp maybe_start_compliance(state) do
    if Build.Server.running?(state, Task.Compliance) do
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
    Build.Server.start_link(__MODULE__, pr, repo_data, nil, name: name(pr))
end
