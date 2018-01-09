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

    defp loop(previous_repo_data \\ nil) do
      repo_data =
        try do
          if Application.get_env(:aircloak_ci, :poll_github, true) do
            repo_data = Github.repo_data("aircloak", "aircloak")
            Enum.each(AircloakCI.RepoDataProvider.subscribers(), &send(&1, {:repo_data, repo_data}))
            ensure_branch_builds(repo_data, previous_repo_data)
            ensure_pr_builds(repo_data)
            repo_data
          else
            previous_repo_data
          end
        catch type, error ->
          Logger.error(Exception.format(type, error, System.stacktrace()))
          nil
        end

      :timer.sleep(:timer.seconds(5))

      loop(repo_data)
    end

    defp ensure_pr_builds(repo_data), do:
      Enum.each(repo_data.pull_requests, &AircloakCI.Build.PullRequest.ensure_started(&1, repo_data))

    defp ensure_branch_builds(repo_data, previous_repo_data), do:
      [Enum.find(repo_data.branches, &(&1.name == "master"))]
      |> Stream.concat(pr_targets(repo_data))
      |> Stream.concat(changed_branches(repo_data, previous_repo_data))
      |> Stream.dedup()
      |> Enum.each(&AircloakCI.Build.Branch.ensure_started(&1, repo_data))

    defp pr_targets(repo_data) do
      traget_branch_names = Enum.map(repo_data.pull_requests, &(&1.target_branch))
      Stream.filter(repo_data.branches, &Enum.member?(traget_branch_names, &1.name))
    end

    defp changed_branches(_repo_data, nil), do:
      # If we don't have previous data, we'll assume there were no changes. This means we'll skip some pushes,
      # but the gain is that we don't keep a large amount of branches in a local cache.
      []
    defp changed_branches(repo_data, previous_repo_data), do:
      Stream.reject(repo_data.branches, &Enum.member?(previous_repo_data.branches, &1))
  end
end
