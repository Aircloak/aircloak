defmodule AircloakCI.RepoDataProvider do
  @moduledoc "Service for providing data about repository."
  use Aircloak.ChildSpec.Supervisor


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Subscribes the caller to the notifications about repository data."
  @spec subscribe() :: :ok
  def subscribe() do
    Registry.register(__MODULE__.Subscribers, :subscriber, nil)
    :ok
  end

  @doc "Retrieves the list of subscribers."
  @spec subscribers() :: [pid]
  def subscribers(), do:
    Enum.map(Registry.lookup(__MODULE__.Subscribers, :subscriber), fn({pid, nil}) -> pid end)


  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  def start_link(), do:
    Supervisor.start_link(
      [
        Aircloak.ChildSpec.registry(:duplicate, __MODULE__.Subscribers),
        __MODULE__.Poller
      ],
      strategy: :one_for_one, name: __MODULE__
    )


  # -------------------------------------------------------------------
  # Poller module
  # -------------------------------------------------------------------

  defmodule Poller do
    @moduledoc false

    use Aircloak.ChildSpec.Task
    alias AircloakCI.Github
    require Logger

    def start_link(), do:
      Task.start_link(&loop/0)

    defp loop() do
      try do
        repo_data = Github.repo_data("aircloak", "aircloak")
        Enum.each(AircloakCI.RepoDataProvider.subscribers(), &send(&1, {:repo_data, repo_data}))
        Enum.each(repo_data.pull_requests, &AircloakCI.Build.PullRequest.ensure_started(&1, repo_data))
      catch type, error ->
        Logger.error(Exception.format(type, error, System.stacktrace()))
      end

      :timer.sleep(:timer.seconds(5))

      loop()
    end
  end
end
