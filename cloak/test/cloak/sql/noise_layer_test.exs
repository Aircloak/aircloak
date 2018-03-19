defmodule Cloak.Sql.NoiseLayer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{NoiseLayer, Expression}

  test "accumulate changes results of evaluating expressions" do
    layers = [
      NoiseLayer.new("alice", [%Expression{row_index: 1}, %Expression{row_index: 2}]),
      NoiseLayer.new("bob", [%Expression{row_index: 0}])
    ]
    processed_layers = NoiseLayer.pre_process_layers(layers)

    accumulator1 = NoiseLayer.new_accumulator(layers)
    accumulator2 = NoiseLayer.accumulate(processed_layers, NoiseLayer.new_accumulator(layers), ["value1", true, 3.4])

    assert Enum.map(accumulator1, &NoiseLayer.sum(&1, "salt")) != Enum.map(accumulator2, &NoiseLayer.sum(&1, "salt"))
  end

  test "merging of accumulators" do
    layers = [
      NoiseLayer.new("alice", [%Expression{row_index: 1}, %Expression{row_index: 2}]),
      NoiseLayer.new("bob", [%Expression{row_index: 0}])
    ]
    processed_layers = NoiseLayer.pre_process_layers(layers)

    accumulator1 = NoiseLayer.new_accumulator(layers)
    accumulator2 = NoiseLayer.accumulate(processed_layers, NoiseLayer.new_accumulator(layers), ["value1", true, 3.4])
    accumulator3 = NoiseLayer.merge_accumulators(accumulator1, accumulator2)

    assert Enum.map(accumulator3, &NoiseLayer.sum(&1, "salt")) == Enum.map(accumulator2, &NoiseLayer.sum(&1, "salt"))
  end
end
