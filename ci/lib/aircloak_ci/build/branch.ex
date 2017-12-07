defmodule AircloakCI.Build.Branch do
  @moduledoc "This module powers the process responsible for the entire build of the single branch."

  use AircloakCI.Build.Server, restart: :temporary
  require Logger
  alias AircloakCI.{Github, Build, LocalProject}
  alias AircloakCI.Build.Task


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
  # Build.Server callbacks
  # -------------------------------------------------------------------

  @impl Build.Server
  def base_branch(%{source: %{name: "master"}}), do: nil
  def base_branch(state), do: Enum.find(state.repo_data.branches, &(&1.name == "master"))

  @impl Build.Server
  def create_project(state), do:
    LocalProject.for_branch(state.source)

  @impl Build.Server
  def refresh_source(state), do:
    Enum.find(state.repo_data.branches, &(&1.name == state.source.name && &1.repo == state.source.repo))

  @impl Build.Server
  def init(nil, state), do:
    {:ok, %{state | data: %{pending_transfers: []}}}

  @impl Build.Server
  def handle_job_succeeded(Task.Compile, state), do: {:noreply, maybe_perform_transfers(state)}

  @impl Build.Server
  def handle_call({:transfer_project, target_project}, from, state), do:
    {:noreply,
      state.data.pending_transfers
      |> update_in(&[{target_project, from} | &1])
      |> maybe_perform_transfers()
    }


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(branch), do:
    {:via, Registry, {AircloakCI.Build.Registry, {:branch, branch.name}}}

  defp maybe_perform_transfers(state) do
    if Build.Server.compiled?(state) do
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
    Build.Server.start_link(__MODULE__, branch, repo_data, nil, name: name(branch))
end
