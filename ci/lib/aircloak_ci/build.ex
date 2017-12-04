defmodule AircloakCI.Build do
  @moduledoc """
  This module powers the process responsible for the entire build of the single PR.

  The process will start various child jobs to initialize the repo and run different tests.
  """

  use AircloakCI.JobRunner.PullRequest, restart: :temporary
  require Logger
  alias AircloakCI.{Github, JobRunner, LocalProject, Queue}
  alias AircloakCI.Job.Compliance


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given pull request is started."
  @spec ensure_started(Github.API.pull_request, Github.API.repo_data) :: :ok
  def ensure_started(pr, repo_data) do
    case Supervisor.start_child(__MODULE__.Supervisor, [pr, repo_data]) do
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
  def handle_restart(state) do
    IO.puts "restarting build"
    {:noreply, prepare_project(state)}
  end

  @impl JobRunner
  def handle_call(:force_build, _from, state) do
    state = JobRunner.terminate_all_jobs(state)
    LocalProject.truncate_logs(state.project)
    LocalProject.set_status(state.project, :force_start)
    {:reply, :ok, prepare_project(state)}
  end


  # -------------------------------------------------------------------
  # Project preparation
  # -------------------------------------------------------------------

  defp prepare_project(%{project: pr_project} = state) do
    base_projects = base_projects(state)
    JobRunner.start_job(
      state,
      :project_preparation,
      fn -> Task.start_link(fn -> run_prepare(pr_project, base_projects) end) end
    )
  end

  defp base_projects(state), do:
    # We're deduping, because if the target is master, we end up with two master branches, which causes deadlocks.
    Enum.uniq([
      LocalProject.for_branch(branch!(state.repo_data, state.source.target_branch)),
      LocalProject.for_branch(branch!(state.repo_data, "master"))
    ])

  defp branch!(repo_data, branch_name), do:
    %{} = Enum.find(repo_data.branches, &(&1.name == branch_name))

  defp run_prepare(pr_project, base_projects), do:
    Queue.exec(project_queue(pr_project), fn -> init_project([pr_project | base_projects]) end)

  defp init_project([project, base_project | rest]) do
    if LocalProject.status(project) == :empty do
      LocalProject.truncate_logs(project)
      # note: no need to queue in `project`, since this is done by the caller
      Queue.exec(project_queue(base_project), fn ->
        :ok = init_project([base_project | rest])
        :ok = LocalProject.initialize_from(project, base_project)
      end)
      :ok = Queue.exec(:compile, fn -> LocalProject.ensure_compiled(project) end)
    else
      :ok
    end
  end
  defp init_project([master_project]), do:
    # note: no need to queue in `master_project`, since this is done by the caller
    :ok = Queue.exec(:compile, fn -> LocalProject.ensure_compiled(master_project) end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(pr), do:
    {:via, Registry, {AircloakCI.Build.Registry, {:pull_request, pr.number}}}

  defp project_queue(project), do: {:project, LocalProject.folder(project)}

  defp maybe_start_ci(state) do
    if LocalProject.ci_possible?(state.project) do
      JobRunner.start_job(
        state,
        :compliance,
        fn -> Compliance.start_link(state.source, state.project, state.repo_data) end
      )
    else
      LocalProject.log(state.project, "main", "can't run compliance on this PR")
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
