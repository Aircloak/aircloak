defmodule Cloak.Sql.NoiseLayer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{NoiseLayer, Expression}

  test "new includes the noise layer names in the accumulators" do
    assert NoiseLayer.new_accumulator([NoiseLayer.new("alice", []), NoiseLayer.new("bob", [])]) ==
      [MapSet.new(["alice"]), MapSet.new(["bob"])]
  end

  test "accumulate appends results of evaluating expressions" do
    layers = [
      NoiseLayer.new("alice", [%Expression{row_index: 1}, %Expression{row_index: 2}]),
      NoiseLayer.new("bob", [%Expression{row_index: 0}])
    ]

    accumulator = [MapSet.new([:old_data]), MapSet.new([:old_data])]

    assert NoiseLayer.accumulate(layers, accumulator, [:value1, :value2, :value3]) ==
      [MapSet.new([:old_data, [:value2, :value3]]), MapSet.new([:old_data, [:value1]])]
  end
end
