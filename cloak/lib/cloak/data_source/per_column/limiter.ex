defmodule Cloak.DataSource.PerColumn.Limiter do
  @moduledoc """
  Controls the execution rate of analysis queries,
  taking into account concurrency, available memory, and delays.
  """

  require Logger

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  def run(fun) do
    :jobs.run(__MODULE__, fn ->
      config = get_config()

      config
      |> Keyword.get(:minimum_memory_required, 0.0)
      |> wait_for_available_memory()

      fun.()

      config
      |> Keyword.get(:time_between_queries, 0)
      |> Process.sleep()

      :ok
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @memory_poll_interval :timer.seconds(1)

  defp wait_for_available_memory(0), do: :ok
  defp wait_for_available_memory(0.0), do: :ok

  defp wait_for_available_memory(threshold) do
    stats = Cloak.MemoryReader.get_stats()
    available = stats.available_memory["last_5_seconds"]
    total = stats.total_memory

    if available / total < threshold do
      Process.sleep(@memory_poll_interval)
      wait_for_available_memory(threshold)
    else
      :ok
    end
  end

  defp get_config(), do: Application.get_env(:cloak, :analysis_queries, [])

  defp setup_queue() do
    with :undefined <- :jobs.queue_info(__MODULE__) do
      concurrency = Keyword.get(get_config(), :concurrency, 3)

      :jobs.add_queue(__MODULE__,
        max_time: :undefined,
        max_size: :undefined,
        regulators: [counter: [limit: concurrency]]
      )
    end
  end

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_arg) do
    Aircloak.ChildSpec.setup_job(&setup_queue/0)
  end
end
