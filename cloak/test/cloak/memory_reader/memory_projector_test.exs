defmodule Cloak.MemoryReader.MemoryProjectorTest do
  use ExUnit.Case, async: true

  alias Cloak.MemoryReader.MemoryProjector

  describe "Adding measurements" do
    test "creates new empty projectors", do:
      assert %MemoryProjector{changes: []} = MemoryProjector.new()

    test "retains last reading" do
      state = MemoryProjector.new()
      |> MemoryProjector.add_reading(1, 1)
      assert %MemoryProjector{last_reading: {1, 1}} = state
    end

    test "maintains a sequence of rates of changes", do:
      assert %MemoryProjector{changes: [1, 1, 1, 1, 1, 1]} = add_measurements([1, 2, 3, 4, 5, 6, 7])

    test "maintains last 20 readings" do
      %MemoryProjector{changes: changes} = add_measurements(1..100)
      assert length(changes) == 20
    end
  end

  describe "Dropping measurements" do
    test "Dropping from an empty buffer has no effect", do:
      assert %MemoryProjector{changes: []} = MemoryProjector.drop(add_measurements([]), 10)

    test "Dropping more measurements than exists empties buffer", do:
      assert %MemoryProjector{changes: []} = MemoryProjector.drop(add_measurements(1..10), 20)

    test "Dropping less measurements than there is free space in the buffer has no effect", do:
      assert %MemoryProjector{changes: [250, 250, 250]} =
        MemoryProjector.drop(add_measurements([500, 750, 1000, 1250]), 3)

    test "Drops the oldest measurements", do:
      assert %MemoryProjector{changes: [250, 250, 250]} =
        MemoryProjector.drop(add_measurements(List.duplicate(250, 20) ++ [500, 750, 1000]), 17)
  end

  describe "Projecting the future" do
    test "projection with insufficient data yields no prediction", do:
      assert :no_prediction == add_measurements(20..10)
      |> MemoryProjector.time_until_limit(0)

    test "detects growth of free memory", do:
      assert :infinity == add_measurements(1..21)
      |> MemoryProjector.time_until_limit(0)

    test "predicts decline in memory", do:
      assert {:ok, 10} == add_measurements(50..20)
      |> MemoryProjector.time_until_limit(10)

    test "weights recent readings higher than older" do
      # This will produce a weighted average of:
      # (-10 * 5 + -20 * 5 + -40 * 4 + 0 * 17 * different weights)/42 = -7.
      # Hence in 1 time unit we will go from 30 to 25.
      measurements = List.duplicate(100, 20) ++ [60, 40, 30]
      assert {:ok, 1} == add_measurements(measurements)
      |> MemoryProjector.time_until_limit(22)
    end
  end

  defp add_measurements(initial_storage \\ MemoryProjector.new(), measurements), do:
     measurements
     |> Enum.with_index()
     |> Enum.reduce(initial_storage, fn({measurement, timestamp}, storage) ->
       MemoryProjector.add_reading(storage, measurement, timestamp)
     end)
end
