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
    merge_state: Github.API.merge_state,
    status_checks: Github.API.statuses,
    approved?: boolean
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Makes sure that the server is running and compiled."
  @spec ensure_started(String.t) :: :ok
  def ensure_started(path) do
    case AircloakCI.Build.Supervisor.start_build(__MODULE__, [path, self()]) do
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
    if pid != nil, do: send(pid, :compiled)
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
      merge_state: :mergeable,
      status_checks: %{},
      approved?: true
    }

  defp empty_repo(), do:
    %{owner: "aircloak", name: "aircloak", branches: [], pull_requests: []}

  defp run_job("cloak_test", state), do: Job.Test.run(state)
  defp run_job("cloak_compile", state), do: Job.Compile.run(state)
  defp run_job("air_compile", state), do: Job.Compile.run(state)
  defp run_job("air_test", state), do: Job.Test.run(state)
  defp run_job("integration_tests_compile", state), do: Job.Compile.run(state)
  defp run_job("integration_tests_test", state), do: Job.Test.run(state)
  defp run_job("compliance", state), do: Job.Compliance.run(state)


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(path, caller), do:
    Build.Server.start_link(__MODULE__, :local, path, empty_repo(), caller, name: name())
end
