defmodule Cloak.MemoryReader.MemoryProjector do
  @moduledoc """
  Allows parsing the information returned when reading /proc/meminfo
  on a linux machine.

  All memory values returned are in kB.
  """

  alias Cloak.MemoryReader.MemoryProjector

  @readings_to_keep 20
  @reading_weights [5, 5, 4, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]

  @type measurement :: non_neg_integer
  @type timestamp :: non_neg_integer

  @type t :: %__MODULE__{
          last_reading: {measurement, timestamp} | nil,
          changes: [integer]
        }

  defstruct last_reading: nil, changes: []

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Creates a new and empty memory projector"
  @spec new() :: t
  def new(), do: %__MODULE__{}

  @doc "Records a memory reading"
  @spec add_reading(t, measurement, timestamp) :: t
  def add_reading(storage, measurement, timestamp) do
    storage
    |> extend_changes(measurement, timestamp)
    |> retain_current_reading(measurement, timestamp)
  end

  @doc """
  Drops a number of changes from the oldest to the most recent.

  The number of items dropped depends on the amount of items in the buffer.
  More specifically the larger of MAX_BUFFER_SIZE - NUM_TO_DROP and 0 items
  of the original buffer are kept.
  """
  @spec drop(t, non_neg_integer) :: t
  def drop(storage, number) do
    num_items_to_keep = max(@readings_to_keep - number, 0)
    %__MODULE__{storage | changes: Enum.take(storage.changes, num_items_to_keep)}
  end

  @doc """
  The number of expected units of time until we reach a given lower memory limit.
  The time units are the same as used when adding new measurements.
  """
  @spec time_until_limit(t, non_neg_integer) ::
          :infinity | :no_prediction | {:ok, non_neg_integer}
  def time_until_limit(%MemoryProjector{changes: changes}, _)
      when length(changes) < @readings_to_keep,
      do: :no_prediction

  def time_until_limit(
        %MemoryProjector{changes: changes, last_reading: {free_memory, _}},
        memory_limit
      ) do
    # We produce a weighted average where the most recent value counts 3 times as much
    # as the oldest. This value is experimentally set, and probably needs furhter adjusting.
    weighted_values_sum =
      changes
      |> Enum.zip(@reading_weights)
      |> Enum.map(fn {value, weight} -> value * weight end)
      |> Enum.sum()

    average_change = div(weighted_values_sum, Enum.sum(@reading_weights))

    if average_change >= 0 do
      :infinity
    else
      {:ok, div(free_memory - memory_limit, abs(average_change))}
    end
  end

  # -------------------------------------------------------------------
  # Internal functions
  # -------------------------------------------------------------------

  defp extend_changes(%MemoryProjector{last_reading: nil} = state, _measurement, _timestamp),
    do: state

  defp extend_changes(
         %MemoryProjector{
           last_reading: {previous_measurement, previous_timestamp},
           changes: changes
         } = state,
         new_measurement,
         new_timestamp
       ) do
    time_since_last_reading = new_timestamp - previous_timestamp
    measured_difference = new_measurement - previous_measurement
    change = div(measured_difference, max(time_since_last_reading, 1))
    %MemoryProjector{state | changes: Enum.take([change | changes], @readings_to_keep)}
  end

  defp retain_current_reading(state, measurement, timestamp),
    do: %MemoryProjector{state | last_reading: {measurement, timestamp}}
end
