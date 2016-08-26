defmodule Air.Supervisor do
  @moduledoc false

  def start_link,
    do: Supervisor.start_link(children, strategy: :one_for_one, name: Air.Supervisor)

  # Conditional definition of top-level processes, since we don't want to run
  # all of them in the test environment.
  case Mix.env do
    :test -> defp children, do: common_processes()
    :dev -> defp children, do: common_processes() ++ system_processes()
    :prod -> defp children, do: common_processes() ++ system_processes()
  end

  # Processes which need to run in all environments (including :test)
  defp common_processes do
    import Supervisor.Spec, warn: false

    [
      Air.ResultProcessor.supervisor_spec(),
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      worker(Air.QueryEvents, []),
      worker(Air.DataSourceManager, []),
      supervisor(Task.Supervisor, [[name: Air.ApiTokenTimestampUpdater]], [id: :api_token_updater]),
      worker(Air.Monitoring.FailedQueries, []),
      Air.ResultProcessor.observer_spec(),
      Air.Endpoint.supervisor_spec(),
    ]
  end

  unless Mix.env == :test do
    # Processes which we don't want to start in the test environment
    defp system_processes do
      import Supervisor.Spec, warn: false

      [
        Air.PeerDiscovery.supervisor_spec()
      ]
    end
  end
end
