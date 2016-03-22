defmodule Cloak do
  @moduledoc false
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    :cluster_cron.init()
    Cloak.Logger.ReportHandler.install()
    :ok = :cloak_alarm_handler.install()

    case Supervisor.start_link(children(), strategy: :one_for_one, name: Cloak.Supervisor) do
      {:ok, pid} ->
        add_webmachine_routes()
        :cloak_metrics_adapter.start_metrics_server()
        {:ok, pid}
      error -> error
    end
  end

  # Conditional definition of top-level processes, since we don't want to run
  # all of them in the test environment.
  case Mix.env do
    :test -> defp children, do: common_processes()
    :dev -> defp children, do: common_processes() ++ system_processes()
    :prod -> defp children, do: common_processes() ++ system_processes()
  end

  defp common_processes do
    import Supervisor.Spec, warn: false

    [
      worker(:db_job, []),
      worker(:progress_handler, []),
      supervisor(:global_service_sup, []),
      supervisor(:cloak_db_pool_sup, []),
      worker(:cloak_db_def, []),
      supervisor(:result_sender_sup, []),
      supervisor(:job_runner_sup, []),
      worker(:queued_worker,
            [:task_coordinator, :task_coordinator, :cloak_conf.get_val(:queries, :concurrent_executions)],
            id: :task_coordinator_queue
          ),
      worker(:webmachine_mochiweb,
            [[
              ip: :cloak_conf.get_val(:api, :address),
              port: :cloak_conf.get_val(:api, :port),
              dispatch: []
            ]],
            function: :start,
            id: :api_endpoint
          )
    ]
  end

  unless Mix.env == :test do
    # Processes which we don't want to start in the test environment
    defp system_processes do
      import Supervisor.Spec, warn: false

      [
        supervisor(:cloak_metrics_sup, []),
        worker(:resource_monitor, []),
        worker(:cron_manager, [])
      ]
    end
  end

  defp add_webmachine_routes do
    [
      # Interface for executing tasks
      {['task', :action], :task_resource, []},
      # Capabilities interface
      {['capabilities'], :capabilities_resource, []}
    ]
    |> Enum.each(&:webmachine_router.add_route/1)
  end
end
