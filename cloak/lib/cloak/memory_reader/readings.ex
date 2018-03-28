defmodule Cloak.MemoryReader.Readings do
  @moduledoc """
  Maintains memory stats a set of time periods.
  The lowest value that has occured within that period is used for reporting.
  """

  @type t :: %__MODULE__{
          level_config: [{String.t(), non_neg_integer}],
          update_counter: non_neg_integer,
          update_max: non_neg_integer,
          values: Map.t()
        }

  defstruct level_config: [], update_counter: 0, update_max: 0, values: Map.new()

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc """
  Creates a new and empty reading.

  It takes a config that declares what levels should be kept,
  and how often values should spill from one level to the next.

  The following config:

    [
      {"current", 1},
      {"last_five_seconds", 20},
      {"last_minute", 12}
    ]

  indicates that we should keep the current reading, as well as a five second minimum
  that contains 20 of the readings at the level above, as well as the minimum value
  within the last minute, which contains the 12 last five second intervals.
  """
  @spec new([{String.t(), non_neg_integer}]) :: t
  def new(config) do
    {adjusted_config, update_max} =
      config
      |> Enum.reduce({[], 1}, fn {level_name, num_records}, {acc, current_num_records} ->
        new_val_each_num_records = num_records * current_num_records

        {
          acc ++ [{level_name, num_records, new_val_each_num_records}],
          new_val_each_num_records
        }
      end)

    %__MODULE__{
      level_config: adjusted_config,
      update_max: update_max
    }
  end

  @doc "Adds a new reading to the readings storage."
  @spec add_reading(t, non_neg_integer) :: t
  def add_reading(storage, reading) do
    values =
      Enum.reduce(storage.level_config, storage.values, fn {name, records, turnover}, values_acc ->
        if rem(storage.update_counter, turnover) == 0 do
          # It's the point where we turn over, add the new value
          Map.update(values_acc, name, [reading], &Enum.take([reading | &1], records))
        else
          # We don't want to add a new reading, but if the new reading is lower
          # than the previous current most recent one, then it should update the
          # latest reading
          Map.update(values_acc, name, [reading], fn [existing_reading | readings] ->
            [min(existing_reading, reading) | readings]
          end)
        end
      end)

    %__MODULE__{
      storage
      | update_counter: rem(storage.update_counter + 1, storage.update_max),
        values: values
    }
  end

  @doc "Returns the lowest value per category, as defined when creating the Readings instance."
  @spec values(t) :: Map.t()
  def values(storage),
    do:
      storage.values
      |> Enum.map(fn {name, values} -> {name, Enum.min(values)} end)
      |> Enum.into(%{})
end
