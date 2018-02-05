defmodule Cloak.MemoryReader.ReadingsTest do
  use ExUnit.Case, async: true

  alias Cloak.MemoryReader.Readings

  describe "Creating a reading struct" do
    test "adjusts layers to account for inserts per change in higher levels" do
      assert %Readings{
        level_config: [
          {"first_level", 1, 1},
          {"second_level", 5, 5},
          {"third_level", 3, 15},
        ]
      } = Readings.new([
        {"first_level", 1},
        {"second_level", 5},
        {"third_level", 3},
      ])
    end

    test "sets the max count property" do
      assert %Readings{
        update_max: 15,
      } = Readings.new([
        {"first_level", 1},
        {"second_level", 5},
        {"third_level", 3},
      ])
    end
  end

  describe "Adding measurements" do
    test "increments counter", do:
      assert %Readings{update_counter: 3} = add_values([1, 2, 3])

    test "for an empty record, adds the value to all levels" do
      assert %Readings{values: values} = add_values([1])
      assert Map.get(values, "first") == [1]
      assert Map.get(values, "second") == [1]
      assert Map.get(values, "fourth") == [1]
    end

    test "replaces the value for higher levels if it's smaller" do
      assert %Readings{values: values} = add_values([2, 1])
      assert Map.get(values, "first") == [1]
      assert Map.get(values, "second") == [1]
      assert Map.get(values, "fourth") == [1]
    end

    test "adds value to higher level when the value should spill over" do
      assert %Readings{values: values} = add_values(8..1)
      assert Map.get(values, "first") == [1]
      assert Map.get(values, "second") == [1, 3]
      assert Map.get(values, "fourth") == [1, 5]
    end
  end

  describe "Reading values" do
    test "returns the lowest per group" do
      assert %{
        "first": 8,
        "fourth": 1,
        "second": 5,
      } = Readings.values(add_values(1..8)) |> Enum.sort()
    end
  end

  defp default_readings() do
    Readings.new([
      {"first", 1},
      {"second", 2},
      {"fourth", 2},
    ])
  end

  defp add_values(storage \\ default_readings(), values), do:
    Enum.reduce(values, storage, & Readings.add_reading(&2, &1))
end
