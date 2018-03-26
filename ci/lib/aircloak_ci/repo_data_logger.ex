defmodule AircloakCI.RepoDataLogger do
  @moduledoc "Logger of retrieved repo data."

  use GenServer
  require Logger

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    AircloakCI.RepoDataProvider.subscribe()
    {:ok, %{branches: nil, pull_requests: nil}}
  end

  @impl GenServer
  def handle_info({:repo_data, repo_data}, state) do
    new_state = %{
      branches: repo_data.branches |> Enum.map(& &1.name) |> Enum.sort() |> Enum.join(", "),
      pull_requests:
        repo_data.pull_requests |> Enum.map(& &1.number) |> Enum.sort() |> Enum.join(", ")
    }

    if new_state.branches != state.branches, do: Logger.info("branches: #{new_state.branches}")

    if new_state.pull_requests != state.pull_requests,
      do: Logger.info("pull_requests: #{new_state.pull_requests}")

    {:noreply, new_state}
  end

  def handle_info(other, state), do: super(other, state)

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  def start_link(_), do: GenServer.start_link(__MODULE__, nil)
end
