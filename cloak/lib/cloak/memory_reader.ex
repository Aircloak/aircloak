defmodule Cloak.MemoryReader do
  @moduledoc """
  Periodically reads the memory available on the server in order to
  determine if we are about to run out of memory or not.

  Once the free memory drops below a certain threshold, it will
  start looking at projections of future memory developments.
  If the memory is deemed to shortly reach critical levels,
  running processes consuming high amounts of memory will be killed.
  """

  use GenServer, start: {__MODULE__, :start_link, []}

  alias Cloak.MemoryReader.{MemoryProjector, PropMemInfo, Readings}

  require Logger

  @large_mem_usage_threshold 500 * 1024 * 1024

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Starts the memory reader server"
  @spec start_link() :: GenServer.on_start()
  def start_link(), do: GenServer.start_link(__MODULE__, [], name: __MODULE__)

  # -------------------------------------------------------------------
  # GenServer callbacks
  # -------------------------------------------------------------------

  @impl GenServer
  def init(_) do
    # We need the memory reader to remain active, even when the system is under high load.
    # Under these circumstances in particular it is important that we are able to kill
    # processes that run rampant to retain a stable `cloak`.
    :erlang.process_flag(:priority, :high)
    params = read_params()

    state = %{
      memory_projector: MemoryProjector.new(),
      params: params,
      last_reading: nil,
      readings:
        Readings.new([
          {"current", 1},
          {"last_5_seconds", 5 * measurements_per_second(%{params: params})},
          {"last_1_minute", 12},
          {"last_5_minutes", 5},
          {"last_15_minutes", 3},
          {"last_1_hour", 4}
        ])
    }

    :timer.send_interval(:timer.seconds(5), :report_memory_stats)
    schedule_check(state)
    {:ok, state}
  end

  @impl GenServer

  def handle_info(:read_memory, %{memory_projector: projector} = state) do
    reading = ProcMemInfo.read()
    time = System.monotonic_time(:millisecond)
    schedule_check(state)

    state
    |> record_reading(reading)
    |> Map.put(
      :memory_projector,
      MemoryProjector.add_reading(projector, reading.available_memory, time)
    )
    |> perform_memory_check(reading.available_memory)
  end

  def handle_info(:report_memory_stats, %{last_reading: nil} = state), do: {:noreply, state}

  def handle_info(:report_memory_stats, state) do
    payload = %{
      total_memory: state.last_reading.total_memory,
      available_memory: Readings.values(state.readings)
    }

    Cloak.AirSocket.send_memory_stats(payload)
    {:noreply, state}
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp perform_memory_check(
         %{params: %{limit_to_start_checks: limit_to_start_checks}} = state,
         available_memory
       )
       when available_memory > limit_to_start_checks,
       do: {:noreply, state}

  defp perform_memory_check(
         %{params: %{limit_to_check_for: memory_limit}} = state,
         available_memory
       )
       when available_memory < memory_limit,
       do: free_memory(state)

  defp perform_memory_check(
         %{
           params: %{limit_to_check_for: memory_limit, allowed_minimum_time_to_limit: time_limit},
           memory_projector: projector
         } = state,
         available_memory
       ) do
    case MemoryProjector.time_until_limit(projector, memory_limit) do
      {:ok, time} when time <= time_limit ->
        Logger.warn(
          "Dangerous memory situation. Anticipating reaching the low memory threshold " <>
            "(#{to_mb(memory_limit)} MB) in #{to_sec(time)} seconds. Available memory: #{to_mb(available_memory)} MB."
        )

        free_memory(state)

      _ ->
        {:noreply, state}
    end
  end

  defp to_mb(bytes), do: trunc(bytes / 1_048_576)

  defp to_sec(ms), do: max(trunc(ms / 1_000), 0)

  defp free_memory(state) do
    oom_killer()

    state = %{
      state
      | # This adds an artificial cool down period between consecutive killings, proportional
        # to the length of the memory projection buffer.
        memory_projector: MemoryProjector.drop(state.memory_projector, num_measurements_to_drop(state))
    }

    {:noreply, state}
  end

  defp schedule_check(%{params: %{check_interval: interval}}), do: Process.send_after(self(), :read_memory, interval)

  def read_params() do
    defaults =
      Application.fetch_env!(:cloak, :memory_limits)
      |> Enum.reduce(%{}, fn {key, value}, map -> Map.put(map, key, value) end)

    config =
      case Aircloak.DeployConfig.fetch(:cloak, "memory_limits") do
        {:ok, analyst_config} ->
          [
            :check_interval,
            :limit_to_start_checks,
            :limit_to_check_for,
            :allowed_minimum_time_to_limit,
            :time_between_abortions
          ]
          |> Enum.reduce(%{}, fn parameter, config ->
            value = Map.get(analyst_config, Atom.to_string(parameter), defaults[parameter])
            Map.put(config, parameter, value)
          end)

        :error ->
          defaults
      end

    Logger.debug(
      "The low memory monitor is configured with: check_interval: #{config.check_interval} ms, " <>
        "limit_to_start_checks: #{config.limit_to_start_checks} bytes, limit_to_check_for: " <>
        "#{config.limit_to_check_for} bytes, allowed_minimum_time_to_limit: " <>
        "#{config.allowed_minimum_time_to_limit} ms, and up to #{config.time_between_abortions} ms " <>
        "of time waited between consecutive query abortions."
    )

    config
  end

  defp record_reading(state, reading) do
    %{
      state
      | last_reading: reading,
        readings: Readings.add_reading(state.readings, reading.available_memory)
    }
  end

  defp measurements_per_second(%{params: %{check_interval: interval}}), do: max(div(1_000, interval), 1)

  defp num_measurements_to_drop(%{
         params: %{check_interval: interval, time_between_abortions: pause}
       }),
       do: div(pause, interval)

  defp oom_killer() do
    Process.list()
    |> Stream.map(&{&1, Process.info(&1, :memory)})
    |> Stream.reject(fn {_pid, state} -> is_nil(state) end)
    |> Enum.max_by(fn {_pid, {:memory, memory}} -> memory end)
    |> case do
      {pid, {:memory, memory}} when memory > @large_mem_usage_threshold ->
        memory_in_mb = div(memory, 1024 * 1024)
        Logger.warn("Killing process #{inspect(pid)}, consuming #{memory_in_mb} MB, because of low memory.")
        Process.exit(pid, :kill)

      _ ->
        Logger.warn("Low memory but no processes found consuming large amounts of memory.")
    end
  end
end
