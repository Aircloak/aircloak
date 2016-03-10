defmodule Air do
  @moduledoc false
  use Application

  # See http://elixir-lang.org/docs/stable/elixir/Application.html
  # for more information on OTP Applications
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    configure_database()

    children = [
      supervisor(Air.Endpoint, []),
      supervisor(Air.Repo, []),
      worker(Air.Repo.Migrator, [], restart: :transient)
    ]

    # See http://elixir-lang.org/docs/stable/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Air.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  def config_change(changed, _new, removed) do
    Air.Endpoint.config_change(changed, removed)
    :ok
  end

  # This function reads database settings from etcd and merges them into the
  # existing repo configuration as specified in `config.exs`. This allows us
  # to change database settings via etcd without needing to bake them into the
  # release.
  defp configure_database() do
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
end
