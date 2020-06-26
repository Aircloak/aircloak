defmodule Central.Supervisor do
  @moduledoc false

  def start_link do
    Supervisor.start_link(
      common_processes() ++ system_processes(),
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  defp common_processes(),
    do: [
      Central.Repo,
      Central.Repo.Migrator,
      Central.Service.License,
      {Phoenix.PubSub, name: CentralWeb.PubSub},
      CentralWeb.Endpoint
    ]

  if Mix.env() == :test do
    defp system_processes(), do: []
  else
    defp system_processes(), do: [Central.Scheduler]
  end
end
