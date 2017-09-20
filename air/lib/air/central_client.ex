defmodule Air.CentralClient do
  @moduledoc "Top-level supervisor of Central client processes."

  use Aircloak.ChildSpec.Supervisor

  @doc "Starts the supervision tree."
  @spec start_link :: Supervisor.on_start
  def start_link() do
    Supervisor.start_link(
      auto_export_process() ++ [
        Supervisor.Spec.worker(Task, [&report_usage/0], id: __MODULE__.UsageReporter, shutdown: :brutal_kill)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  defp auto_export_process() do
    if Air.Service.Central.auto_export?(),
      do: [Air.CentralClient.Socket],
      else: []
  end

  defp report_usage(), do:
    report_usage_loop(:erlang.monotonic_time(:seconds))

  defp report_usage_loop(previous_time) do
    :timer.sleep(Application.fetch_env!(:air, :usage_report_interval))
    now = :erlang.monotonic_time(:seconds)
    send_usage_info(now - previous_time)
    report_usage_loop(now)
  end

  defp send_usage_info(seconds_online), do:
    Air.Service.Central.send_usage_info(%{
      air_utc_time: NaiveDateTime.utc_now(),
      air_version: Aircloak.Version.for_app(:air) |> Aircloak.Version.to_string(),
      seconds_online: seconds_online,
      cloaks: Enum.map(Air.Service.Cloak.all_cloak_infos(), &Map.take(&1, [:name, :data_source_ids, :version]))
    })
end
