defmodule Cloak.Query.AnonimyzerTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Anonymizer

  test "an anonymizer needs at least one noise layer" do
    assert_raise(FunctionClauseError, fn -> Anonymizer.new([]) end)
  end

  test "sufficiently_large?" do
    assert Anonymizer.new([MapSet.new()]) |> Anonymizer.sufficiently_large?(20)
    refute Anonymizer.new([MapSet.new()]) |> Anonymizer.sufficiently_large?(2)
  end

  describe "aggregators return nil on too few users" do
    test "count" do
      # per-user row format = count of values
      rows = [10, 10, 10]
      # Count doesn't return nil in any SQL dialect we are aware of.
      # We therefore return the lowest possible returnable value instead.
      assert {0, nil} = Anonymizer.new([MapSet.new()]) |> Anonymizer.count(rows)
    end

    test "sum" do
      # per-user row format = sum of values
      rows = [10, 10, 10, -10, -10, -10]
      assert {nil, nil} = Anonymizer.new([MapSet.new()]) |> Anonymizer.sum(rows)
    end

    test "avg" do
      # per-user row format = {:avg, sum of values, count of values}
      rows = [{:avg, 10, 1}, {:avg, 10, 1}, {:avg, 10, 1}]
      assert {nil, nil} = Anonymizer.new([MapSet.new()]) |> Anonymizer.avg(rows)
    end

    test "stddev" do
      # per-user row format = {:stddev, sum of values, sum of squared values, count of values}
      rows = [{:stddev, 1, 1, 1}, {:stddev, 2, 4, 1}]
      assert {nil, nil} = Anonymizer.new([MapSet.new()]) |> Anonymizer.stddev(rows)
    end
  end

  test "count" do
    # per-user row format = count of values
    rows = [1, 2, 1, 0, 3, 1, 2, 3, 1, 2, 1]
    assert {12, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.count(rows)
  end

  test "sum of sufficient values" do
    # per-user row format = sum of values
    rows = [1, 2, -1, 0, 1, 2, -4, -2, 4, 1, -1, 2]
    assert {6.0, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.sum(rows)
  end

  test "sum produces value, even when not enough negative values to produce negative sum" do
    # per-user row format = sum of values
    rows = [10, 10, 10, 10, 10, -10, -10]
    assert {50.0, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.sum(rows)
  end

  test "sum produces value, even when not enough positive values to produce positive sum" do
    # per-user row format = sum of values
    rows = [10, 10, -10, -10, -10, -10, -10]
    assert {-50.0, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.sum(rows)
  end

  test "avg" do
    # per-user row format = {:avg, sum of values, count of values}
    rows = [
      {:avg, 1, 1},
      {:avg, 2, 1},
      {:avg, -1, 1},
      {:avg, 0, 1},
      {:avg, 1, 1},
      {:avg, 2, 2},
      {:avg, -4, 4},
      {:avg, -2, 1},
      {:avg, 4, 3},
      {:avg, -1, 2}
    ]

    assert {0.3, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.avg(rows)
  end

  test "stddev" do
    # per-user row format = {:stddev, sum of values, sum of squared values, count of values}
    rows = [
      {:stddev, 1, 1, 1},
      {:stddev, 2, 4, 1},
      {:stddev, -1, 1, 1},
      {:stddev, 0, 0, 1},
      {:stddev, 1, 1, 1},
      {:stddev, 2, 2, 2},
      {:stddev, -4, 4, 4},
      {:stddev, -2, 4, 1},
      {:stddev, 4, 6, 3}
    ]

    assert {0.8988882021697692, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.stddev(rows)
  end

  test "min" do
    # per-user row format = {:min, min value}
    rows = [
      {:min, -2},
      {:min, 3},
      {:min, -3},
      {:min, -2},
      {:min, -1},
      {:min, -3},
      {:min, 3},
      {:min, -2},
      {:min, 5},
      {:min, -4},
      {:min, -5},
      {:min, 3},
      {:min, -6}
    ]

    assert -2 = Anonymizer.new([MapSet.new()]) |> Anonymizer.min(rows) |> round()
  end

  test "max" do
    # per-user row format = {:max, max value}
    rows = [
      {:max, 1},
      {:max, 3},
      {:max, 4},
      {:max, 4},
      {:max, -1},
      {:max, -2},
      {:max, 3},
      {:max, 4},
      {:max, 5},
      {:max, -4},
      {:max, 4},
      {:max, 3},
      {:max, 3}
    ]

    assert 3 = Anonymizer.new([MapSet.new()]) |> Anonymizer.max(rows) |> round()
  end

  test "median" do
    # per-user row format = collection of all values
    rows = [
      [1, 2],
      [3],
      [4, 2, -3],
      [2, 4],
      [0],
      [-3, -2],
      [3],
      [4, -2, 1],
      [5],
      [-4],
      [-5, 4],
      [3],
      [-1, 2],
      [4],
      [5, -2, 3],
      [-2, -1],
      [2],
      [-1, -3],
      [-3],
      [2, 6, 3],
      [-6],
      [4],
      [5, -4],
      [-3]
    ]

    assert 2 = Anonymizer.new([MapSet.new()]) |> Anonymizer.median(rows) |> round()
  end

  test "same noise layers are collapsed" do
    noise_layer = MapSet.new([3, 4])
    anonymizer1 = Anonymizer.new([noise_layer])
    anonymizer2 = Anonymizer.new([noise_layer, noise_layer])

    assert anonymizer1.rngs == anonymizer2.rngs
  end

  test "min/max/median/avg sanity check" do
    data = [1, -1, -10, 40, 2, 5, 6, 6, 7, 10, 10, -2, 12, -6, 7, 6, 1, 9]
    anonymizer = Anonymizer.new([MapSet.new()])
    min = Anonymizer.min(anonymizer, Enum.map(data, &{:min, &1}))
    max = Anonymizer.max(anonymizer, Enum.map(data, &{:max, &1}))
    median = Anonymizer.median(anonymizer, Enum.flat_map(data, &[[&1], [&1]]))
    assert min < median
    assert median < max
  end

  test "min/max/median return nil on insuficient data" do
    data = [1, 1, 2, 5, 6, 6, 7, 10, 10, 12]
    anonymizer = Anonymizer.new([MapSet.new()])
    assert anonymizer |> Anonymizer.min(Enum.map(data, &{:min, &1})) |> is_nil()
    assert anonymizer |> Anonymizer.max(Enum.map(data, &{:max, &1})) |> is_nil()
    assert anonymizer |> Anonymizer.median(Enum.map(data, &[&1])) |> is_nil()
  end
end
