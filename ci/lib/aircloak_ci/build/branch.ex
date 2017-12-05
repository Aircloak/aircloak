defmodule AircloakCI.Build.Branch do
  @moduledoc "This module powers the process responsible for the entire build of the single branch."

  use AircloakCI.JobRunner.Branch, restart: :temporary
  require Logger
  alias AircloakCI.{Github, JobRunner, LocalProject, Queue}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given branch is started."
  @spec ensure_started(Github.API.branch, Github.API.repo_data) :: :ok
  def ensure_started(branch, repo_data) do
    case AircloakCI.Build.Supervisor.start_build(__MODULE__, [branch, repo_data]) do
      {:ok, _} -> :ok
      {:error, {:already_started, _pid}} -> :ok
    end
  end

  @doc """
  Transfers the branch project to another location.

  This is useful when initializing a new project which is based on some other branch. For example a PR is based on the
  target branch. With transferring, we start with some artifact already available. Git repo is mostly up to date, and
  projects also might already be compiled.
  """
  @spec transfer_project(Github.API.branch, LocalProject.t) :: :ok
  def transfer_project(branch, target_project), do:
    GenServer.call(name(branch), {:transfer_project, target_project}, :timer.minutes(30))


  # -------------------------------------------------------------------
  # JobRunner callbacks
  # -------------------------------------------------------------------

  @impl JobRunner
  def init(nil, state), do:
    {:ok, prepare_project(%{state | data: %{pending_transfers: []}})}

  @impl JobRunner
  def handle_restart(state), do:
    {:noreply, prepare_project(state)}

  @impl JobRunner
  def handle_job_succeeded(:compilation, state), do:
    {:noreply, maybe_perform_transfers(state)}

  @impl JobRunner
  def handle_call({:transfer_project, target_project}, from, state), do:
    {:noreply,
      state.data.pending_transfers
      |> update_in(&[{target_project, from} | &1])
      |> maybe_perform_transfers()
    }


  # -------------------------------------------------------------------
  # Project preparation
  # -------------------------------------------------------------------

  defp prepare_project(%{project: project} = state), do:
    JobRunner.start_job(
      state,
      :compilation,
      fn -> Task.start_link(fn -> initialize_project_repo(project) end) end
    )

  defp initialize_project_repo(project) do
    with :ok <- LocalProject.initialize(project), do:
      Queue.exec(:compile, fn -> LocalProject.ensure_compiled(project) end)
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(branch), do:
    {:via, Registry, {AircloakCI.Build.Registry, {:branch, branch.name}}}

  defp maybe_perform_transfers(state) do
    if Enum.member?(JobRunner.running_jobs(state), :compilation) do
      state
    else
      state.data.pending_transfers
      |> Enum.reverse()
      |> Enum.each(fn({target_project, from}) -> transfer_project(state, target_project, from) end)

      put_in(state.data.pending_transfers, [])
    end
  end

  defp transfer_project(state, target_project, from) do
    LocalProject.initialize_from(target_project, state.project)
    GenServer.reply(from, :ok)
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(branch, repo_data), do:
    JobRunner.start_link(__MODULE__, branch, repo_data, nil, name: name(branch))
end
