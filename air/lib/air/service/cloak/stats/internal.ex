defmodule Air.Service.Cloak.Stats.Internal do
  @moduledoc """
  Internal stats implementation for handling memory and query
  stats from Cloaks.
  """

  @type cloak_id :: String.t()
  @type raw_memory_reading :: %{
          total_memory: number,
          available_memory: %{
            current: number,
            last_5_seconds: number,
            last_1_minute: number,
            last_5_minutes: number,
            last_15_minutes: number,
            last_1_hour: number
          }
        }
  @type stats :: %{
          memory: %{
            total: number,
            currently_in_use: number,
            in_use_percent: number,
            readings: [number]
          },
          queries: [number]
        }
  @type cloak_stats :: %{cloak_id => stats}
  @type state :: %{
          stats: cloak_stats,
          pending_memory_readings: %{cloak_id => [raw_memory_reading]},
          pending_queries: %{cloak_id => number}
        }

  # An hours worth of data at 10 second interval
  @timesteps_to_keep 360

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Initial empty memory reading"
  @spec initial_state() :: state
  def initial_state(), do: %{stats: %{}, pending_memory_readings: %{}, pending_queries: %{}}

  @doc "Adds a cloak to the state with default empty data"
  @spec register(state, cloak_id()) :: state
  def register(state, cloak_id),
    do:
      state
      |> put_in([:stats, cloak_id], initial_memory_stats())
      |> put_in([:pending_queries, cloak_id], 0)

  @doc "Removes a cloak from internal state"
  @spec unregister(state, cloak_id) :: state
  def unregister(state, cloak_id) do
    state
    |> update_in([:stats], &Map.delete(&1, cloak_id))
    |> update_in([:pending_memory_readings], &Map.delete(&1, cloak_id))
    |> update_in([:pending_queries], &Map.delete(&1, cloak_id))
  end

  @doc "Records cloak memory readings"
  @spec record_memory(state, cloak_id, raw_memory_reading) :: state
  def record_memory(state, cloak_id, reading) do
    if registered?(state, cloak_id) do
      state
      |> update_in([:stats, cloak_id], &update_base_memory_stats(&1, reading))
      |> update_in([:pending_memory_readings], fn pending_memory_readings ->
        Map.update(pending_memory_readings, cloak_id, [reading], &[reading | &1])
      end)
    else
      state
    end
  end

  @doc "Records a query as having been run"
  @spec record_query(state, cloak_id) :: state
  def record_query(state, cloak_id) do
    if registered?(state, cloak_id) do
      update_in(state.pending_queries, fn pending_queries ->
        Map.update(pending_queries, cloak_id, 1, &(&1 + 1))
      end)
    else
      state
    end
  end

  @doc "Processes and aggregating all memory readings, taking the max memory reading"
  @spec process(state) :: state
  def process(state),
    do:
      state
      |> process_pending(:pending_memory_readings, &process_memory/2)
      |> process_pending(:pending_queries, &process_queries/2)

  @doc "Returns stats for all the cloaks"
  @spec cloak_stats(state) :: cloak_stats
  def cloak_stats(%{stats: stats}), do: stats

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp process_pending(state, what, callback), do: Enum.reduce(state[what], Map.put(state, what, %{}), callback)

  defp process_memory({cloak_id, readings}, acc) do
    max_in_use_percentage =
      readings
      |> Enum.map(&base_stats(&1).in_use_percent)
      |> Enum.max()

    update_in(acc, [:stats, cloak_id, :memory, :readings], &add_to_list_of_readings(&1, max_in_use_percentage))
  end

  defp process_queries({cloak_id, num_queries}, acc),
    do: update_in(acc, [:stats, cloak_id, :queries], &add_to_list_of_readings(&1, num_queries))

  defp update_base_memory_stats(stats, reading),
    do:
      update_in(stats.memory, fn mem_stats ->
        stats = base_stats(reading)
        %{mem_stats | total: stats.total, currently_in_use: stats.in_use, in_use_percent: stats.in_use_percent}
      end)

  defp initial_memory_stats(),
    do: %{
      memory: %{
        total: 0,
        currently_in_use: 0,
        in_use_percent: 0,
        readings: List.duplicate(0, @timesteps_to_keep)
      },
      queries: List.duplicate(0, @timesteps_to_keep)
    }

  defp add_to_list_of_readings(readings, reading), do: Enum.take([reading | readings], @timesteps_to_keep)

  defp base_stats(reading) do
    total = reading.total_memory
    in_use = total - reading.available_memory.current
    in_use_percent = round(in_use * 100 / total)

    %{total: total, in_use: in_use, in_use_percent: in_use_percent}
  end

  defp registered?(%{stats: stats}, cloak_id), do: Map.has_key?(stats, cloak_id)
end
