defmodule AircloakCI.PullRequestProvider do
  @moduledoc "Service for providing data about pull requests."
  use Aircloak.ChildSpec.Supervisor


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Subscribes the caller to the notifications about pull requests."
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
    Supervisor.start_link(processes(), strategy: :one_for_one, name: __MODULE__)

  if Mix.env == :prod do
    defp processes(), do: [registry_spec(), __MODULE__.Poller]
  else
    # We won't start the poller in dev/test to avoid needless GH API requests.
    defp processes(), do: [registry_spec()]
  end

  defp registry_spec(), do: Aircloak.ChildSpec.registry(:duplicate, __MODULE__.Subscribers)


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
        Enum.each(
          AircloakCI.PullRequestProvider.subscribers(),
          &send(&1, {:current_pull_requests, Github.pending_pull_requests("aircloak", "aircloak")})
        )
      catch type, error ->
        Logger.error(Exception.format(type, error, System.stacktrace()))
      end

      :timer.sleep(:timer.seconds(5))
    end
  end
end
