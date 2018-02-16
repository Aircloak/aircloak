defmodule Central.Supervisor do
  @moduledoc false

  def start_link, do:
    Supervisor.start_link(
      [
        Central.Repo,
        Central.Repo.Migrator,
        Central.Service.Customer,
        CentralWeb.Endpoint,
      ],
      strategy: :one_for_one, name: __MODULE__
    )
end
