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
        worker(Task, [&report_usage/0], id: Module.concat(__MODULE__, UsageReporter), shutdown: :brutal_kill)
      ],
      strategy: :one_for_one,
      name: __MODULE__
    )
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
    Air.CentralClient.Socket.send_usage_info(%{
      air_utc_time: NaiveDateTime.utc_now(),
      air_version: Aircloak.Version.for_app(:air) |> Aircloak.Version.to_string(),
      seconds_online: seconds_online,
      cloaks: Enum.map(Air.DataSourceManager.cloaks(), &Map.take(&1, [:name, :data_source_ids, :version]))
    })
end
