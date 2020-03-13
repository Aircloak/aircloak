defmodule AircloakCI.Build.Branch do
  @moduledoc "This module powers the process responsible for the entire build of the single branch."

  use AircloakCI.Build.Server, restart: :temporary
  require Logger
  alias AircloakCI.{Build, Github, LocalProject}

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given branch is started."
  @spec ensure_started(Github.API.branch(), Github.API.repo_data()) :: pid
  def ensure_started(branch, repo_data) do
    case AircloakCI.Build.Supervisor.start_build(__MODULE__, [branch, repo_data]) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end

  @doc """
  Transfers the branch project to another location.

  This is useful when initializing a new project which is based on some other branch. For example a PR is based on the
  target branch. With transferring, we start with some artifact already available. Git repo is mostly up to date, and
  projects also might already be compiled.
  """
  @spec transfer_project(Github.API.branch(), LocalProject.t()) :: :ok
  def transfer_project(branch, target_project),
    do: GenServer.call(name(branch), {:transfer_project, target_project}, :timer.minutes(30))

  # -------------------------------------------------------------------
  # Build.Server callbacks
  # -------------------------------------------------------------------

  @impl Build.Server
  def run_nightly?(state), do: build_branch?(state)

  @impl Build.Server
  def build_source(branch_name, repo_data) do
    branch = find_branch(repo_data, branch_name)

    if is_nil(branch) do
      nil
    else
      %{
        source: branch,
        base_branch: if(branch_name == "master", do: nil, else: find_branch(repo_data, "master")),
        project: LocalProject.for_branch(branch)
      }
    end
  end

  @impl Build.Server
  def init(nil, state), do: {:ok, %{state | data: %{pending_transfers: []}}}

  @impl Build.Server
  def handle_job_succeeded("prepare", state) do
    if state.source.name == "master", do: ensure_database_containers(state)

    # we're always compiling target branches, because they serve as a base (cache) for pull requests
    state = if build_branch?(state), do: Build.Job.Compile.start_if_possible(state), else: state

    {:noreply, maybe_perform_transfers(state)}
  end

  def handle_job_succeeded(_job, state), do: {:noreply, maybe_perform_transfers(state)}

  @impl Build.Server
  def handle_job_failed(_job, _reason, state), do: {:noreply, maybe_perform_transfers(state)}

  @impl Build.Server
  def handle_call({:transfer_project, target_project}, from, state),
    do:
      {:noreply,
       state.data.pending_transfers
       |> update_in(&[{target_project, from} | &1])
       |> maybe_perform_transfers()}

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  # A build branch is distinguished from other branches because we're running compilation jobs on every change. This
  # allows us to have cached latest compiled artifacts and docker images. The list of build branches is small, and
  # it includes the master branch, release branches, and some other long-running branches.
  #
  # In contrast, no jobs are started on non-build branches. A typical example is a feature branch. There's no point in
  # running any jobs on such branch, since they will be executed on the corresponding PR build.
  defp build_branch?(state), do: state.source.name in ~w(master) or state.source.name =~ ~r/^release_\d+$/

  defp find_branch(repo_data, branch_name), do: Enum.find(repo_data.branches, &(&1.name == branch_name))

  defp name(branch), do: {:via, Registry, {AircloakCI.Build.Registry, {:branch, branch.name}}}

  defp maybe_perform_transfers(state) do
    if Build.Server.running_jobs(state) == [] do
      state.data.pending_transfers
      |> Enum.reverse()
      |> Enum.each(fn {target_project, from} -> transfer_project(state, target_project, from) end)

      put_in(state.data.pending_transfers, [])
    else
      state
    end
  end

  defp transfer_project(state, target_project, from) do
    LocalProject.initialize_from(target_project, state.project)
    GenServer.reply(from, :ok)
  end

  defp ensure_database_containers(state) do
    with {:error, error} <- LocalProject.exec_container_script(state.project, "cloak", ["ensure_database_containers"]),
         do: Logger.error("error starting compliance containers: #{error}")
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(branch, repo_data),
    do:
      Build.Server.start_link(
        __MODULE__,
        :branch,
        branch.name,
        repo_data,
        nil,
        name: name(branch)
      )
end
