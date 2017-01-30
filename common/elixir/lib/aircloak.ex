defmodule Aircloak do
  @moduledoc false
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [supervisor(Aircloak.ProcessMonitor, [])],
      strategy: :one_for_one, name: Aircloak.Supervisor
    )
  end
end
