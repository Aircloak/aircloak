defmodule Air.Service.Cloak.StatsReporter do
  @moduledoc """
  Service that periodically triggers stats aggregation and reports stats
  to actively listening clients.
  """
  alias Air.Service.Cloak.Stats

  use GenServer

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    schedule_next_cycle()
    {:ok, nil}
  end

  @impl GenServer
  def handle_info(:trigger_aggregation_and_report, state) do
    schedule_next_cycle()

    Stats.aggregate()

    stats =
      Enum.map(
        Air.Service.Cloak.all_cloak_infos(),
        &Map.take(&1, [:id, :name, :online_since, :stats])
      )

    AirWeb.Socket.Frontend.CloakStatsChannel.broadcast_cloak_stats(stats)

    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp schedule_next_cycle(), do: Process.send_after(self(), :trigger_aggregation_and_report, :timer.seconds(10))
end
