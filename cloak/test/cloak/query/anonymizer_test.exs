defmodule Cloak.Query.AnonimyzerTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Anonymizer
  alias Cloak.Sql.NoiseLayer

  defp empty_accumulator(), do: NoiseLayer.accumulator_from_values([])

  test "an anonymizer needs at least one noise layer" do
    assert_raise(FunctionClauseError, fn -> Anonymizer.new([]) end)
  end

  test "sufficiently_large?" do
    assert empty_accumulator() |> Anonymizer.new() |> Anonymizer.sufficiently_large?(20)
    refute empty_accumulator() |> Anonymizer.new() |> Anonymizer.sufficiently_large?(2)
  end

  describe "aggregators return absolute lower bound on too few users" do
    test "count" do
      # per-user row format = count of values
      rows = [10, 10, 10]
      # Count doesn't return nil in any SQL dialect we are aware of.
      # We therefore return the lowest possible returnable value instead.
      assert {2, nil} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.count(rows)
    end

    test "sum" do
      # per-user row format = sum of values
      rows = [10, 10, 10, -10, -10, -10]
      assert {nil, nil} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.sum(rows)
    end

    test "avg" do
      # per-user row format = {:avg, sum of values, count of values}
      rows = [{:avg, 10, 1}, {:avg, 10, 1}, {:avg, 10, 1}]
      assert {nil, nil} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.avg(rows)
    end

    test "variance" do
      # per-user row format = {:variance, sum of values, sum of squared values, count of values}
      rows = [{:variance, 1, 1, 1}, {:variance, 2, 4, 1}]
      assert {nil, nil} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.variance(rows)
    end
  end

  test "count" do
    # per-user row format = count of values
    rows = [1, 2, 1, 0, 3, 1, 2, 3, 1, 2, 1]
    assert {12, 0.0} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.count(rows)
  end

  test "sum of sufficient values" do
    # per-user row format = sum of values
    rows = [1, 2, -1, 0, 1, 2, -4, -2, 4, 1, -1, 2]
    assert {6.0, 0.0} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.sum(rows)
  end

  test "sum produces value, even when not enough negative values to produce negative sum" do
    # per-user row format = sum of values
    rows = [10, 10, 10, 10, 10, -10, -10]
    assert {50.0, 0.0} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.sum(rows)
  end

  test "sum produces value, even when not enough positive values to produce positive sum" do
    # per-user row format = sum of values
    rows = [10, 10, -10, -10, -10, -10, -10]
    assert {-50.0, 0.0} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.sum(rows)
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

    assert {0.3, 0.0} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.avg(rows)
  end

  test "variance" do
    # per-user row format = {:variance, sum of values, sum of squared values, count of values}
    rows = [
      {:variance, 1, 1, 1},
      {:variance, 2, 4, 1},
      {:variance, -1, 1, 1},
      {:variance, 0, 0, 1},
      {:variance, 1, 1, 1},
      {:variance, 2, 2, 2},
      {:variance, -4, 4, 4},
      {:variance, -2, 4, 1},
      {:variance, 4, 6, 3}
    ]

    assert {0.8079999999999998, 0.0} = empty_accumulator() |> Anonymizer.new() |> Anonymizer.variance(rows)
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

    assert -2 = empty_accumulator() |> Anonymizer.new() |> Anonymizer.min(rows) |> round()
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

    assert 3 = empty_accumulator() |> Anonymizer.new() |> Anonymizer.max(rows) |> round()
  end

  test "same noise layers are collapsed" do
    noise_layer = NoiseLayer.accumulator_from_values([3, 4])
    anonymizer1 = Anonymizer.new(noise_layer)
    anonymizer2 = Anonymizer.new(noise_layer ++ noise_layer)

    assert anonymizer1.rngs == anonymizer2.rngs
  end

  test "min/max sanity check" do
    data = [1, -1, -10, 40, 2, 5, 6, 6, 7, 10, 10, -2, 12, -6, 7, 6, 1, 9]
    anonymizer = empty_accumulator() |> Anonymizer.new()
    min = Anonymizer.min(anonymizer, Enum.map(data, &{:min, &1}))
    max = Anonymizer.max(anonymizer, Enum.map(data, &{:max, &1}))
    assert min < max
  end

  test "min/max return nil on insuficient data" do
    data = [1, 1, 2, 5, 6, 6, 7, 10, 10, 12]
    anonymizer = empty_accumulator() |> Anonymizer.new()
    assert anonymizer |> Anonymizer.min(Enum.map(data, &{:min, &1})) |> is_nil()
    assert anonymizer |> Anonymizer.max(Enum.map(data, &{:max, &1})) |> is_nil()
  end

  test "noisy statistics" do
    statistics = {10, 100, 1, 20, 10, 5}
    anonymizer = empty_accumulator() |> Anonymizer.new()
    assert {sum, min, max, sd} = Anonymizer.noisy_statistics(anonymizer, statistics)
    assert_in_delta sum, 100, 1
    assert_in_delta min, 1, 1
    assert_in_delta max, 20, 1
    assert_in_delta sd, 10, 1
  end

  test "statistics aggregators bounds around 0" do
    anonymizer = empty_accumulator() |> Anonymizer.new()

    assert {_sum, 0.0, _max, _sd} = Anonymizer.noisy_statistics(anonymizer, {5, 15, 1, 10, 3, 4})
    assert {_sum, _min, 0.0, _sd} = Anonymizer.noisy_statistics(anonymizer, {5, -15, -10, -1, -3, 4})
    assert {0.0, _min, _max, _sd} = Anonymizer.noisy_statistics(anonymizer, {5, 0.1, 0, 0.02, 0.1, 0.01})
  end

  test "insufficient statistics" do
    statistics = {2, 100, 1, 20, 10, 5}
    anonymizer = empty_accumulator() |> Anonymizer.new()
    assert {nil, nil, nil, nil} = Anonymizer.noisy_statistics(anonymizer, statistics)
  end
end
