defmodule Air.CentralClient do
  @moduledoc "Top-level supervisor of Central client processes."

  @doc "Starts the supervision tree."
  @spec start_link :: Supervisor.on_start
  def start_link() do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [
        worker(Air.CentralClient.Socket, []),
        worker(Air.CentralClient.QueryReporter, []),
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end
end
