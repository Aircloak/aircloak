defmodule Cloak.Sql.NoiseLayer.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{NoiseLayer, Expression}

  defp sum(accumulator), do: Enum.map(accumulator, &NoiseLayer.sum(&1, "salt"))

  defp new_accumulator(layers, rows) do
    processed_layers = NoiseLayer.pre_process_layers(layers)

    Enum.reduce(
      rows,
      NoiseLayer.new_accumulator(layers),
      &NoiseLayer.accumulate(processed_layers, &2, &1)
    )
  end

  test "accumulate changes results of evaluating expressions" do
    layers = [
      NoiseLayer.new("alice", [%Expression{row_index: 1}, %Expression{row_index: 2}]),
      NoiseLayer.new("bob", [%Expression{row_index: 0}])
    ]

    accumulator1 = new_accumulator(layers, [])
    accumulator2 = new_accumulator(layers, [["value1", true, 3.4]])

    assert sum(accumulator1) != sum(accumulator2)
  end

  test "acc() + acc(data) = acc(data)" do
    layers = [
      NoiseLayer.new("alice", [%Expression{row_index: 1}, %Expression{row_index: 2}]),
      NoiseLayer.new("bob", [%Expression{row_index: 0}])
    ]

    accumulator1 = new_accumulator(layers, [])
    accumulator2 = new_accumulator(layers, [["value1", true, 3.4]])
    accumulator3 = NoiseLayer.merge_accumulators(accumulator1, accumulator2)

    assert sum(accumulator3) == sum(accumulator2)
  end

  test "acc(data1) + acc(data2) = acc(data1 + data2)" do
    layers = [NoiseLayer.new("xxx", [%Expression{row_index: 0}, %Expression{row_index: 2}])]

    accumulator1 = new_accumulator(layers, [["value1", false, 0]])
    accumulator2 = new_accumulator(layers, [["value2", true, 3.4]])
    accumulator3 = new_accumulator(layers, [["value1", false, 0], ["value2", true, 3.4]])

    assert sum(NoiseLayer.merge_accumulators(accumulator1, accumulator2)) == sum(accumulator3)
  end

  test "acc(data1 + data2) = acc(data2 + data1)" do
    layers = [NoiseLayer.new("xxx", [%Expression{row_index: 0}, %Expression{row_index: 2}])]

    accumulator1 = new_accumulator(layers, [["value1", false, 0], ["value2", true, 3.4]])
    accumulator2 = new_accumulator(layers, [["value2", true, 3.4], ["value1", false, 0]])

    assert sum(accumulator1) == sum(accumulator2)
  end

  test "acc(data1) != acc(data2)" do
    layers = [NoiseLayer.new("xxx", [%Expression{row_index: 0}, %Expression{row_index: 2}])]

    accumulator1 = new_accumulator(layers, [["value1", true, 3.4]])
    accumulator2 = new_accumulator(layers, [["value2", true, 3.4]])

    assert sum(accumulator1) != sum(accumulator2)
  end

  test "acc(data) == acc(data)" do
    layers = [NoiseLayer.new("xxx", [%Expression{row_index: 0}, %Expression{row_index: 2}])]

    accumulator1 = new_accumulator(layers, [["value", true, 3.4]])
    accumulator2 = new_accumulator(layers, [["value", false, 3.4]])

    assert sum(accumulator1) == sum(accumulator2)
  end

  test "acc(data1 + data1) = acc(data1)" do
    layers = [NoiseLayer.new("xxx", [%Expression{row_index: 0}, %Expression{row_index: 2}])]

    accumulator1 = new_accumulator(layers, [["value1", false, 0], ["value1", true, 0]])
    accumulator2 = new_accumulator(layers, [["value1", false, 0]])

    assert sum(accumulator1) == sum(accumulator2)
  end
end
