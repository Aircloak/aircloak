defmodule Cloak.MemoryUsage do
  require Logger

  defp total() do
    memory_usage = :erlang.memory()

    stats =
      [:total, :processes, :ets, :binary]
      |> Stream.map(&"#{&1}=#{memory_usage |> Keyword.fetch!(&1) |> div(1024 * 1024)} MB")
      |> Enum.join(", ")

    Logger.info("memory usage: #{stats}")
  end

  def child_spec(_) do
    Aircloak.ChildSpec.supervisor(
      [total_reader()],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  defp total_reader(),
    do: {Periodic, run: &total/0, every: :timer.minutes(1), overlap?: false, timeout: :timer.minutes(1)}
end
