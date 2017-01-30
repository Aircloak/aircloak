defmodule Central.Supervisor do
  @moduledoc false

  def start_link do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Central.Repo, []),
      worker(Central.Repo.Migrator, [], restart: :transient),
      supervisor(Central.AirStats, []),
      supervisor(Central.Endpoint, [], function: :start_site),
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: Central.Supervisor)
  end
end
