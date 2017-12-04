defmodule AircloakCI.Build do
  @moduledoc "Functions for interacting with builds."
  use Aircloak.ChildSpec.Supervisor
  alias AircloakCI.Github


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Ensures that the build server for the given pull request is started."
  @spec ensure_started(Github.API.pull_request, Github.API.repo_data) :: Supervisor.on_start_child
  def ensure_started(pr, repo_data), do:
    Supervisor.start_child(__MODULE__.Supervisor, [pr, repo_data])

  @doc "Force starts the build of the given pull request."
  @spec force_build(Github.API.pull_request, Github.API.repo_data) :: :ok | {:error, String.t}
  def force_build(pr, repo_data) do
    ensure_started(pr, repo_data)
    AircloakCI.Build.Server.force_build(pr)
  end


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link() do
    import Aircloak.ChildSpec

    Supervisor.start_link(
      [
        registry(:unique, __MODULE__.Registry),
        supervisor([AircloakCI.Build.Server], strategy: :simple_one_for_one, name: __MODULE__.Supervisor)
      ],
      strategy: :one_for_one, name: __MODULE__
    )
  end
end
