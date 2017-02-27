defmodule Cloak.MemoryReader do
  @moduledoc """
  Periodically reads the memory available on the server in order to
  determine if we are about to run out of memory or not.

  Once the free memory drops below a certain threshold, it will
  start looking at projections of future memory developments.
  If the memory is deemed to shortly reach critical levels,
  running queries will be cancelled.
  """

  use GenServer

  alias Cloak.MemoryReader.MemoryProjector

  require Logger

  @type query_killer_callbacks :: {(() -> :ok), (() -> :ok)}

  @query_kill_cooloff_cycles 10


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
      # It's bumped on every kill, and then slowly reduced over time.
      # This causes there to be a pause between query killings as to avoid
      # a massacre.
      kill_cooloff: 0,
    }
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
    |> Map.put(:memory_projector, MemoryProjector.add_reading(projector, free_memory, time))
    |> reduce_kill_cooloff()
    |> perform_memory_check(free_memory)
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

  defp kill_query(%{kill_cooloff: cooloff} = state) when cooloff > 0, do:
    {:noreply, state}
  defp kill_query(%{queries: []} = state), do:
    {:noreply, state}
  defp kill_query(%{queries: [query | queries]} = state) do
    Cloak.Query.Runner.stop(query, :oom)
    {:noreply, %{state | queries: queries, kill_cooloff: @query_kill_cooloff_cycles}}
  end

  defp schedule_check(%{params: %{check_interval: interval}}), do: Process.send_after(self(), :read_memory, interval)

  def read_params() do
    defaults = Application.fetch_env!(:cloak, :memory_limits)
    |> Enum.reduce(%{}, fn({key, value}, map) -> Map.put(map, key, value) end)

    config = case Aircloak.DeployConfig.fetch(:cloak, "memory_limits") do
      {:ok, analyst_config} ->
        [:check_interval, :limit_to_start_checks, :limit_to_check_for, :allowed_minimum_time_to_limit]
        |> Enum.reduce(%{}, fn(parameter, config) ->
          value = Map.get(analyst_config, Atom.to_string(parameter), defaults[parameter])
          Map.put(config, parameter, value)
        end)
      :error -> defaults
    end

    Logger.debug("The low memory monitor is configured with: check_interval: #{config.check_interval} ms, " <>
      "limit_to_start_checks: #{config.limit_to_start_checks} bytes, limit_to_check_for: " <>
      "#{config.limit_to_check_for} bytes, and allowed_minimum_time_to_limit: " <>
      "#{config.allowed_minimum_time_to_limit} ms")
    config
  end

  defp reduce_kill_cooloff(%{kill_cooloff: 0} = state), do: state
  defp reduce_kill_cooloff(%{kill_cooloff: cooloff} = state), do: %{state | kill_cooloff: cooloff - 1}
end
