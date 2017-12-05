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
        ensure_branch_builds(repo_data)
        ensure_pr_builds(repo_data)
      catch type, error ->
        Logger.error(Exception.format(type, error, System.stacktrace()))
      end

      :timer.sleep(:timer.seconds(5))

      loop()
    end

    defp ensure_pr_builds(repo_data), do:
      Enum.each(repo_data.pull_requests, &AircloakCI.Build.PullRequest.ensure_started(&1, repo_data))

    defp ensure_branch_builds(repo_data) do
      pr_targets = pr_targets(repo_data)

      repo_data.branches
      |> Stream.filter(&Enum.member?(pr_targets, %{name: &1.name, repo: &1.repo}))
      # ensure that master branch is started first
      |> Enum.sort_by(&(if &1.name == "master", do: {0, &1}, else: {1, &1}))
      |> Enum.each(&AircloakCI.Build.Branch.ensure_started(&1, repo_data))
    end

    defp pr_targets(repo_data), do:
      repo_data.pull_requests
      |> Stream.map(&%{name: &1.target_branch, repo: &1.repo})
      |> Enum.dedup()
  end
end
