defmodule Cloak.MemoryUsage do
  @moduledoc "Periodical logging of some memory usage stats."

  require Logger

  @interval :timer.minutes(1)
  @large_mem_usage_in_mb 100

  # -------------------------------------------------------------------
  # Total memory
  # -------------------------------------------------------------------

  @doc false
  def total() do
    memory_usage = :erlang.memory()

    stats =
      [:total, :processes, :ets, :binary]
      |> Stream.map(&"#{&1}=#{memory_usage |> Keyword.fetch!(&1) |> bytes_to_mb()} MB")
      |> Enum.join(", ")

    Logger.info("memory usage: #{stats}")
  end

  # -------------------------------------------------------------------
  # Large processes
  # -------------------------------------------------------------------

  @doc false
  def processes() do
    Process.list()
    |> Stream.map(&{&1, Process.info(&1, [:memory, :registered_name, :initial_call])})
    |> Stream.reject(fn {_pid, info} -> is_nil(info) end)
    |> Stream.map(fn {pid, info} -> info |> Map.new() |> Map.put(:pid, pid) end)
    |> Stream.filter(&(bytes_to_mb(&1.memory) >= @large_mem_usage_in_mb))
    |> Enum.sort_by(& &1.memory, &>=/2)
    |> Enum.each(&log_process/1)
  end

  defp log_process(process) do
    name = with [] <- process.registered_name, do: process.pid

    with {:current_stacktrace, stacktrace} <- Process.info(process.pid, :current_stacktrace) do
      stacktrace = Cloak.LoggerTranslator.filtered_stacktrace(stacktrace)

      [
        "memory usage: process #{name} uses #{bytes_to_mb(process.memory)} MB",
        "initial_call: #{inspect(process.initial_call)}",
        "stacktrace: #{inspect(stacktrace)}"
      ]
      |> Enum.join(", ")
      |> Logger.info()
    end
  end

  # -------------------------------------------------------------------
  # Large ETS tables
  # -------------------------------------------------------------------

  @doc false
  def ets() do
    :ets.all()
    |> Stream.map(&:ets.info/1)
    |> Stream.reject(&(&1 == :undefined))
    |> Stream.map(&Map.new/1)
    |> Stream.filter(&(words_to_mb(&1.memory) >= @large_mem_usage_in_mb))
    |> Enum.sort_by(& &1.memory, &>=/2)
    |> Enum.each(&Logger.info("memory usage: ETS table #{&1.name} uses #{words_to_mb(&1.memory)} MB"))
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp bytes_to_mb(bytes), do: div(bytes, 1024 * 1024)

  defp words_to_mb(words), do: bytes_to_mb(words * :erlang.system_info(:wordsize))

  # -------------------------------------------------------------------
  # Supervision tree
  # -------------------------------------------------------------------

  @doc false
  def child_spec(_) do
    Aircloak.ChildSpec.supervisor(
      [reader(:total), reader(:processes), reader(:ets)],
      strategy: :one_for_one,
      name: __MODULE__
    )
  end

  defp reader(fun),
    do: {Periodic, id: fun, run: {__MODULE__, fun, []}, every: @interval, overlap?: false, timeout: 2 * @interval}
end
