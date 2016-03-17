defmodule Cloak do
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      # Define workers and child supervisors to be supervised
      # worker(Cloak.Worker, [arg1, arg2, arg3]),
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
          )
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Cloak.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
