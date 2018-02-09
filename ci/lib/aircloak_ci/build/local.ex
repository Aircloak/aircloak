defmodule AircloakCI.Build.Local do
  @moduledoc "This module can be used to run desired jobs in the current local project."

  use AircloakCI.Build.Server, restart: :temporary
  require Logger
  alias AircloakCI.{Build, CmdRunner, Github, LocalProject}

  @type source :: %{
    path: String.t,
    repo: Github.API.repo,
    sha: String.t,
    merge_sha: String.t,
    merge_state: Github.API.merge_state,
    approved?: boolean
  }


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Makes sure that the server is running and compiled."
  @spec ensure_started(String.t) :: pid
  def ensure_started(path) do
    case AircloakCI.Build.Supervisor.start_build(__MODULE__, [path, self()]) do
      {:ok, pid} -> pid
      {:error, {:already_started, pid}} -> pid
    end
  end


  # -------------------------------------------------------------------
  # Build.Server callbacks
  # -------------------------------------------------------------------

  @impl Build.Server
  def build_source(path, repo_data), do:
    %{source: source(path, repo_data), base_branch: nil, project: LocalProject.for_local(path)}

  @impl Build.Server
  def init(pid, state), do:
    {:ok, %{state | data: pid}}


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp name(), do:
    {:via, Registry, {AircloakCI.Build.Registry, :local}}

  defp source(path, repo_data), do:
    %{
      path: path,
      repo: %{owner: repo_data.owner, name: repo_data.name},
      sha: String.trim(CmdRunner.run_with_output!("git rev-parse HEAD")),
      merge_sha: String.trim(CmdRunner.run_with_output!("git rev-parse HEAD")),
      merge_state: :mergeable,
      approved?: true
    }

  defp empty_repo(), do:
    %{owner: "aircloak", name: "aircloak", branches: [], pull_requests: []}


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(path, caller), do:
    Build.Server.start_link(__MODULE__, :local, path, empty_repo(), caller, name: name())
end
