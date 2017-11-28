defmodule AircloakCI.RepoDataProvider do
  @moduledoc "Service for providing data about repository."
  use Aircloak.ChildSpec.Supervisor


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Subscribes the caller to the notifications about repository data."
  @spec subscribe(String.t) :: :ok
  def subscribe(role) do
    Registry.register(__MODULE__.Subscribers, :subscriber, role)
    :ok
  end

  @doc "Returns the list of subscriber pids."
  @spec subscriber_pids() :: [pid]
  def subscriber_pids(), do:
    Enum.map(subscribers(), fn({pid, _role}) -> pid end)

  @doc "Returns the roles of known subscribers."
  @spec subscriber_roles() :: [String.t]
  def subscriber_roles(), do:
    Enum.map(subscribers(), fn({_pid, role}) -> role end)


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp subscribers(), do:
    Registry.lookup(__MODULE__.Subscribers, :subscriber)


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
      Task.start_link(fn -> loop(false) end)

    defp loop(terminating?) do
      try do
        if terminating?, do: maybe_terminate(), else: provide_data()
      catch type, error ->
        Logger.error(Exception.format(type, error, System.stacktrace()))
      end

      :timer.sleep(:timer.seconds(5))
      if not terminating? and AircloakCI.soft_termination?(), do: start_termination()
      loop(AircloakCI.soft_termination?())
    end

    defp provide_data() do
      repo_data = Github.repo_data("aircloak", "aircloak")
      Enum.each(AircloakCI.RepoDataProvider.subscriber_pids(), &send(&1, {:repo_data, repo_data}))
      Enum.each(repo_data.pull_requests, &AircloakCI.Build.ensure_started(&1, repo_data))
    end

    defp start_termination() do
      Logger.info("starting soft termination")
      log_remaining_subscribers()
      Enum.each(AircloakCI.RepoDataProvider.subscriber_pids(), &send(&1, :soft_terminate))
    end

    defp maybe_terminate() do
      if Enum.empty?(AircloakCI.RepoDataProvider.subscriber_roles()) do
        Logger.info("flushing pending Github requests")
        Github.soft_terminate()
        Logger.info("terminating with non-zero exit to trigger systemd restart")
        Logger.flush()
        System.halt(1)
      else
        log_remaining_subscribers()
      end
    end

    defp log_remaining_subscribers(), do:
      Enum.each(
        AircloakCI.RepoDataProvider.subscriber_roles(),
        &Logger.info("awaiting termination of #{&1}")
      )
  end
end
