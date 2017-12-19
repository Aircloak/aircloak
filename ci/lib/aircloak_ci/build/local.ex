defmodule AircloakCI.Build.Local do
  @moduledoc "This module can be used to run desired jobs in the current local project."

  use AircloakCI.Build.Server, restart: :temporary
  require Logger
  alias AircloakCI.{Build, Github, LocalProject}
  alias AircloakCI.Build.Job

  @type source :: %{
    path: String.t,
    repo: Github.API.repo,
    sha: String.t,
    merge_sha: String.t,
    mergeable?: true,
    status_checks: Github.API.statuses,
    approved?: true
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Makes sure that the server is running and compiled."
  @spec ensure_started() :: :ok
  def ensure_started() do
    case AircloakCI.Build.Supervisor.start_build(__MODULE__, ["..", self()]) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end

    receive do :compiled -> :ok end
  end

  @doc "Starts the given job."
  @spec run_job(Build.Server.job_name) :: :ok
  def run_job(job_name), do:
    GenServer.call(name(), {:run_job, job_name})


  # -------------------------------------------------------------------
  # Build.Server callbacks
  # -------------------------------------------------------------------

  @impl Build.Server
  def build_source(path, repo_data), do:
    %{source: source(path, repo_data), base_branch: nil, project: LocalProject.for_local(path)}

  @impl Build.Server
  def init(pid, state), do:
    {:ok, %{state | data: pid}}

  @impl Build.Server
  def handle_job_succeeded("compile", %{data: pid} = state) do
    send(pid, :compiled)
    {:noreply, %{state | data: nil}}
  end
  def handle_job_succeeded(other, state), do: super(other, state)

  @impl Build.Server
  def handle_call({:run_job, job_name}, _from, state) do
    LocalProject.mark_forced(state.project, job_name)
    {:reply, :ok, run_job(job_name, state)}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(), do:
    {:via, Registry, {AircloakCI.Build.Registry, :local}}

  defp source(path, repo_data), do:
    %{
      path: path,
      repo: %{owner: repo_data.owner, name: repo_data.name},
      sha: String.trim(to_string(:os.cmd('git rev-parse HEAD'))),
      merge_sha: String.trim(to_string(:os.cmd('git rev-parse HEAD'))),
      mergeable?: true,
      status_checks: %{"continuous-integration/travis-ci/pr" => %{status: :success, description: ""}},
      approved?: true
    }

  defp empty_repo(), do:
    %{owner: "aircloak", name: "aircloak", branches: [], pull_requests: []}

  defp run_job("cloak_test", state), do: Job.Test.run(state)
  defp run_job("cloak_compile", state), do: Job.Compile.run(state)
  defp run_job("compliance", state), do: Job.Compliance.run(state)


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(path, caller), do:
    Build.Server.start_link(__MODULE__, :local, path, empty_repo(), caller, name: name())
end
