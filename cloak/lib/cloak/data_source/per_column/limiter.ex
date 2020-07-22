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
      time_between_queries = config[:time_between_queries]
      minimum_memory_required = config[:minimum_memory_required]

      Process.sleep(div(time_between_queries, 2))
      wait_for_available_memory(minimum_memory_required)
      fun.()
      Process.sleep(div(time_between_queries, 2))
      :ok
    end)
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  @memory_poll_interval :timer.seconds(5)

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

  defp get_config(), do: Application.fetch_env!(:cloak, :analysis_queries)

  defp setup_queue() do
    with :undefined <- :jobs.queue_info(__MODULE__) do
      config = get_config()
      concurrency = config[:concurrency]

      :jobs.add_queue(__MODULE__,
        max_time: :undefined,
        max_size: :undefined,
        regulators: [counter: [limit: if(concurrency > 0, do: concurrency, else: 3)]]
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
