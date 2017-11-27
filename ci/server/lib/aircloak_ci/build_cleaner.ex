defmodule AircloakCI.BuildCleaner do
  @moduledoc "Process which cleans up old build folders."

  use GenServer, start: {__MODULE__, :start_link, []}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    AircloakCI.RepoDataProvider.subscribe()
    {:ok, nil}
  end

  @impl GenServer
  def handle_info({:repo_data, repo_data}, state) do
    AircloakCI.LocalProject.remove_old_folders(repo_data)
    AircloakCI.Job.Queue.remove_needless_project_queues()
    {:noreply, state}
  end
  def handle_info(message, state), do:
    super(message, state)


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def start_link(), do:
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
end
