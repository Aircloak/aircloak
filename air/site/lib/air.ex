defmodule Air do
  @moduledoc false
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    configure_database()
    configure_endpoint()
    Supervisor.start_link(children(), strategy: :one_for_one, name: Air.Supervisor)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Air.Endpoint.config_change(changed, removed)
    :ok
  end

  # Conditional definition of top-level processes, since we don't want to run
  # all of them in test environment
  case Mix.env do
    :test -> defp children, do: common_processes()
    :dev -> defp children, do: common_processes() ++ system_processes()
    :prod -> defp children, do: common_processes() ++ system_processes()
  end

  # Processes which need to run in all environments (including :test)
  defp common_processes do
    import Supervisor.Spec, warn: false

    [
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient),
      supervisor(Air.Endpoint, [])
    ]
  end

  unless Mix.env == :test do
    # Processes which need to run only when the system is started
    defp system_processes do
      import Supervisor.Spec, warn: false

      [
        Air.PeerDiscovery.supervisor_spec()
      ]
    end
  end

  # This function reads database settings from etcd and merges them into the
  # existing repo configuration as specified in `config.exs`. This allows us
  # to change database settings via etcd without needing to bake them into the
  # release.
  defp configure_database do
    static_repo_config = Application.get_env(:air, Air.Repo, [])
    runtime_repo_config = Keyword.merge(static_repo_config,
          hostname: :air_etcd.get("/settings/air/db/host"),
          port: String.to_integer(:air_etcd.get("/settings/air/db/port")),
          ssl: String.to_existing_atom(:air_etcd.get("/settings/air/db/ssl")),
          database: :air_etcd.get("/settings/air/db/insights_database"),
          username: :air_etcd.get("/settings/air/db/username"),
          password: :air_etcd.get("/settings/air/db/password"),
        )
    Application.put_env(:air, Air.Repo, runtime_repo_config)
  end

  # Adapts the endpoint configuration to runtime conditions. This is basically needed
  # to support running multiple instances of the app on the dev machine.
  defp configure_endpoint do
    endpoint_config = Application.get_env(:air, Air.Endpoint, [])
    port_offset = String.to_integer(System.get_env("INSIGHTS_PORT_OFFSET") || "0")
    if port_offset > 0 do
      runtime_endpoint_config = update_in(endpoint_config, [:http, :port],
          fn(port_base) -> port_base + port_offset end)
      Application.put_env(:air, Air.Endpoint, runtime_endpoint_config)
    end
  end
end
