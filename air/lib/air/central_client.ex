defmodule Air.CentralClient do
  @moduledoc "Top-level supervisor of Central client processes."

  use Aircloak.ChildSpec.Supervisor

  @doc "Starts the supervision tree."
  @spec start_link :: Supervisor.on_start
  def start_link(), do:
    Supervisor.start_link(auto_export_process(), strategy: :one_for_one, name: __MODULE__)

  defp auto_export_process() do
    if Air.Service.Central.auto_export?(),
      do: [Air.CentralClient.Socket],
      else: []
  end
end
