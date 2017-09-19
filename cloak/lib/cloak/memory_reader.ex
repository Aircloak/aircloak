defmodule Cloak.MemoryReader do
  @moduledoc """
  Periodically reads the memory available on the server in order to
  determine if we are about to run out of memory or not.

  Once the free memory drops below a certain threshold, it will
  start looking at projections of future memory developments.
  If the memory is deemed to shortly reach critical levels,
  running queries will be cancelled.
  """

  use GenServer, start: {__MODULE__, :start_link, []}

  alias Cloak.MemoryReader.{MemoryProjector, Readings}

  require Logger

  @type query_killer_callbacks :: {(() -> :ok), (() -> :ok)}


  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the memory reader server"
  @spec start_link() :: GenServer.on_start
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  @doc "Registers a query such that it can later be killed in case of a low memory event"
  @spec query_registering_callbacks() :: query_killer_callbacks
  def query_registering_callbacks() do
    query_pid = self()
    {
      fn() -> GenServer.cast(__MODULE__, {:register_query, query_pid}) end,
      fn() -> GenServer.cast(__MODULE__, {:unregister_query, query_pid}) end
    }
  end


  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @doc false
  def init(_) do
    state = %{
      memory_projector: MemoryProjector.new(),
      queries: [],
      params: read_params(),
      last_reading: nil,
      readings: Readings.new([
        {"current", 1},
        {"last_5_seconds", 5 * measurements_per_second(%{params: read_params()})},
        {"last_1_minute", 12},
        {"last_5_minutes", 5},
        {"last_15_minutes", 15},
        {"last_1_hour", 4},
      ]),
    }
    :timer.send_interval(:timer.seconds(5), :report_memory_stats)
    schedule_check(state)
    {:ok, state}
  end

  @doc false
  def handle_cast({:unregister_query, pid}, %{queries: queries} = state), do:
    {:noreply, %{state | queries: Enum.reject(queries, & &1 == pid)}}
  def handle_cast({:register_query, pid}, %{queries: queries} = state) do
    Process.monitor(pid)
    {:noreply, %{state | queries: [pid | queries]}}
  end

  @doc false
  def handle_info({:DOWN, _monitor_ref, :process, pid, _info}, %{queries: queries} = state), do:
    {:noreply, %{state | queries: Enum.reject(queries, & &1 == pid)}}
  def handle_info(:read_memory, %{memory_projector: projector} = state) do
    reading = :memsup.get_system_memory_data()
    time = System.monotonic_time(:millisecond)
    schedule_check(state)
    free_memory = Keyword.get(reading, :free_memory)
    state
    |> record_reading(reading)
    |> Map.put(:memory_projector, MemoryProjector.add_reading(projector, free_memory, time))
    |> perform_memory_check(free_memory)
  end
  def handle_info(:report_memory_stats, %{last_reading: nil} = state), do:
    {:noreply, state}
  def handle_info(:report_memory_stats, state) do
    payload = %{
      total_memory: Keyword.get(state.last_reading, :total_memory),
      free_memory: Readings.values(state.readings),
    }
    Cloak.AirSocket.send_memory_stats(payload)
    {:noreply, state}
  end


  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp perform_memory_check(%{params: %{limit_to_start_checks: limit_to_start_checks}} = state, free_memory)
      when free_memory > limit_to_start_checks, do:
    {:noreply, state}
  defp perform_memory_check(%{params: %{limit_to_check_for: memory_limit}} = state, free_memory)
      when free_memory < memory_limit, do:
    kill_query(state)
  defp perform_memory_check(%{params: %{limit_to_check_for: memory_limit, allowed_minimum_time_to_limit: time_limit},
      memory_projector: projector} = state, free_memory) do
    case MemoryProjector.time_until_limit(projector, memory_limit) do
      {:ok, time} when time <= time_limit ->
        Logger.error("Dangerous memory situation. Anticipating reaching the low memory threshold " <>
          "(#{to_mb(memory_limit)} MB) in #{to_sec(time)} seconds. Free memory: #{to_mb(free_memory)} MB")
        kill_query(state)
      _ -> {:noreply, state}
    end
  end

  defp to_mb(bytes), do: trunc(bytes / 1_048_576)

  defp to_sec(ms), do: max(trunc(ms / 1_000), 0)

  defp kill_query(%{queries: []} = state), do:
    {:noreply, state}
  defp kill_query(%{queries: [query | queries]} = state) do
    Cloak.Query.Runner.stop(query, :oom)
    state = %{state |
      # This adds an artificial cool down period between consecutive killings, proportional
      # to the length of the memory projection buffer.
      memory_projector: MemoryProjector.drop(state.memory_projector, num_measurements_to_drop(state)),
      queries: queries,
    }
    {:noreply, state}
  end

  defp schedule_check(%{params: %{check_interval: interval}}), do: Process.send_after(self(), :read_memory, interval)

  def read_params() do
    defaults = Application.fetch_env!(:cloak, :memory_limits)
    |> Enum.reduce(%{}, fn({key, value}, map) -> Map.put(map, key, value) end)

    config = case Aircloak.DeployConfig.fetch(:cloak, "memory_limits") do
      {:ok, analyst_config} ->
        [
          :check_interval, :limit_to_start_checks, :limit_to_check_for,
          :allowed_minimum_time_to_limit, :time_between_abortions
        ]
        |> Enum.reduce(%{}, fn(parameter, config) ->
          value = Map.get(analyst_config, Atom.to_string(parameter), defaults[parameter])
          Map.put(config, parameter, value)
        end)
      :error -> defaults
    end

    Logger.debug("The low memory monitor is configured with: check_interval: #{config.check_interval} ms, " <>
      "limit_to_start_checks: #{config.limit_to_start_checks} bytes, limit_to_check_for: " <>
      "#{config.limit_to_check_for} bytes, allowed_minimum_time_to_limit: " <>
      "#{config.allowed_minimum_time_to_limit} ms, and up to #{config.time_between_abortions} ms " <>
      "of time waited between consecutive query abortions.")
    config
  end

  defp record_reading(state, reading) do
    %{state |
      last_reading: reading,
      readings: Readings.add_reading(state.readings, Keyword.get(reading, :free_memory)),
    }
  end

  defp measurements_per_second(%{params: %{check_interval: interval}}), do: max(div(1_000, interval), 1)

  defp num_measurements_to_drop(%{params: %{check_interval: interval, time_between_abortions: pause}}), do:
    div(pause, interval)
end
