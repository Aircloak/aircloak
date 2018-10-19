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
  @type memory_stats :: %{
          memory: %{
            total: number,
            currently_in_use: number,
            in_use_percent: number,
            readings: [number]
          }
        }
  @type stats :: %{cloak_id => memory_stats}
  @type state :: %{
          stats: %{cloak_id => memory_stats},
          pending_memory_readings: %{cloak_id => [raw_memory_reading]}
        }

  # An hours worth of data at 10 second interval
  @timesteps_to_keep 360

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Initial empty memory reading"
  @spec initial_state() :: state
  def initial_state(), do: %{stats: %{}, pending_memory_readings: %{}}

  @doc "Adds a cloak to the state with default empty data"
  @spec register(state, cloak_id()) :: state
  def register(%{stats: stats} = state, cloak_id),
    do: %{state | stats: Map.put(stats, cloak_id, initial_memory_stats())}

  @doc "Records cloak memory readings"
  @spec record_memory(state, cloak_id, raw_memory_reading) :: state
  def record_memory(state, cloak_id, reading),
    do:
      state
      |> update_in([:stats, cloak_id], &update_base_memory_stats(&1, reading))
      |> update_in([:pending_memory_readings], fn pending_memory_readings ->
        Map.update(pending_memory_readings, cloak_id, [reading], &[reading | &1])
      end)

  @doc "Processes and aggregating all memory readings, taking the max memory reading"
  @spec process(state) :: state
  def process(state) do
    state.pending_memory_readings
    |> Enum.reduce(state, fn
      {_cloak_id, []}, acc ->
        acc

      {cloak_id, readings}, acc ->
        max_in_use_percentage =
          readings
          |> Enum.map(fn reading ->
            {_, _, in_use_percent} = base_stats(reading)
            in_use_percent
          end)
          |> Enum.max()

        acc
        |> update_in([:stats, cloak_id, :memory, :readings], &add_to_list_of_readings(&1, max_in_use_percentage))
        |> put_in([:pending_memory_readings, cloak_id], [])
    end)
  end

  @doc "Returns stats for all the cloaks"
  @spec cloak_stats(state) :: stats
  def cloak_stats(%{stats: stats}), do: stats

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp update_base_memory_stats(stats, reading),
    do:
      update_in(stats, [:memory], fn mem_stats ->
        {total, in_use, in_use_percent} = base_stats(reading)
        %{mem_stats | total: total, currently_in_use: in_use, in_use_percent: in_use_percent}
      end)

  defp initial_memory_stats(),
    do: %{
      memory: %{
        total: 0,
        currently_in_use: 0,
        in_use_percent: 0,
        readings: List.duplicate(0, @timesteps_to_keep)
      }
    }

  defp add_to_list_of_readings(readings, reading), do: Enum.take([reading | readings], @timesteps_to_keep)

  defp base_stats(reading) do
    total = reading.total_memory
    in_use = total - reading.available_memory.current
    in_use_percent = in_use * 100 / total

    {total, in_use, in_use_percent}
  end
end
