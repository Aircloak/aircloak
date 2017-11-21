defmodule AircloakCI.BuildCleaner do
  @moduledoc "Process which cleans up old build folders."

  use GenServer, start: {__MODULE__, :start_link, []}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @impl GenServer
  def init(nil) do
    AircloakCI.PullRequestProvider.subscribe()
    {:ok, nil}
  end

  @impl GenServer
  def handle_info({:current_pull_requests, pull_requests}, state) do
    AircloakCI.Build.remove_old_folders(pull_requests)
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
