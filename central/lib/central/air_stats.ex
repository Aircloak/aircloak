defmodule Central.AirStats do
  @moduledoc "Starts processes for gathering and logging of air statistics."


  # -------------------------------------------------------------------
  # Api functions
  # -------------------------------------------------------------------

  @doc "Starts the supervisor of connection monitors."
  @spec start_link() :: Supervisor.on_start
  def start_link() do
    import Supervisor.Spec, warn: false

    Supervisor.start_link(
      [
        worker(Registry, [:unique, Central.AirStats.Registry]),
        supervisor(Task.Supervisor, [[name: Central.AirStats.TaskSup]]),
        supervisor(Central.AirStats.ConnectionMonitor, []),
      ],
      strategy: :rest_for_one
    )
  end
end
