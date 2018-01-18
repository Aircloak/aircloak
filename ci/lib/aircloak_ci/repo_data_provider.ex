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
        __MODULE__.Poller,
        AircloakCI.RepoDataLogger,
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
        if Application.get_env(:aircloak_ci, :poll_github, true) do
          repo_data = Github.repo_data("aircloak", "aircloak")
          Enum.each(AircloakCI.RepoDataProvider.subscribers(), &send(&1, {:repo_data, repo_data}))
          ensure_branch_builds(repo_data)
          ensure_pr_builds(repo_data)
        end
      catch type, error ->
        Logger.error(Exception.format(type, error, System.stacktrace()))
      end

      :timer.sleep(:timer.seconds(5))
      loop()
    end

    defp ensure_pr_builds(repo_data), do:
      Enum.each(repo_data.pull_requests, &AircloakCI.Build.PullRequest.ensure_started(&1, repo_data))

    defp ensure_branch_builds(repo_data), do:
      [Enum.find(repo_data.branches, &(&1.name == "master" or &1.name =~ ~r/^release_\d+$/))]
      |> Stream.concat(pr_targets(repo_data))
      |> Stream.dedup()
      |> Enum.each(&AircloakCI.Build.Branch.ensure_started(&1, repo_data))

    defp pr_targets(repo_data) do
      target_branch_names = Enum.map(repo_data.pull_requests, &(&1.target_branch))
      Stream.filter(repo_data.branches, &Enum.member?(target_branch_names, &1.name))
    end
  end
end
