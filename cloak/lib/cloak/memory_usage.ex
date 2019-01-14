defmodule Cloak.MemoryUsage do
  require Logger

  defp total() do
    memory_usage = :erlang.memory()

    stats =
      [:total, :processes, :ets, :binary]
      |> Stream.map(&"#{&1}=#{memory_usage |> Keyword.fetch!(&1) |> to_mb()} MB")
      |> Enum.join(", ")

    Logger.info("memory usage: #{stats}")
  end

  defp processes() do
    Process.list()
    |> Stream.map(&{&1, Process.info(&1, [:memory, :registered_name, :initial_call])})
    |> Stream.reject(fn {_pid, info} -> is_nil(info) end)
    |> Stream.map(fn {pid, info} -> info |> Map.new() |> Map.put(:pid, pid) end)
    |> Stream.filter(&(to_mb(&1.memory) >= 1))
    |> Enum.sort_by(& &1.memory, &>=/2)
    |> Enum.each(&log_process/1)
  end

  defp log_process(process) do
    name = with [] <- process.registered_name, do: process.pid

    with {:current_stacktrace, stacktrace} <- Process.info(process.pid, :current_stacktrace) do
      stacktrace = Cloak.LoggerTranslator.filtered_stacktrace(stacktrace)

      [
        "memory usage: process #{name} uses #{to_mb(process.memory)} MB",
        "initial_call: #{inspect(process.initial_call)}",
        "stacktrace: #{inspect(stacktrace)}"
      ]
      |> Enum.join(", ")
      |> Logger.info()
    end
  end

  defp to_mb(bytes), do: div(bytes, 1024 * 1024)

  def child_spec(_) do
    Aircloak.ChildSpec.supervisor(
      [total_reader(), processes_reader()],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  defp total_reader() do
    {Periodic, run: &total/0, every: :timer.minutes(1), overlap?: false, timeout: :timer.minutes(1), id: :total}
  end

  defp processes_reader() do
    {Periodic, run: &processes/0, every: :timer.minutes(1), overlap?: false, timeout: :timer.minutes(1), id: :processes}
  end
end
