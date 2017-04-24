defmodule Cloak.Query.AnonimyzerTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Anonymizer

  test "an anonymizer needs at least one noise layer" do
    assert_raise(FunctionClauseError, fn() -> Anonymizer.new([]) end)
  end

  test "sufficiently_large?" do
    assert {true, _} = Anonymizer.new([MapSet.new()]) |> Anonymizer.sufficiently_large?(20)
    assert {false, _} = Anonymizer.new([MapSet.new()]) |> Anonymizer.sufficiently_large?(2)
  end

  test "count" do
    # per-user row format = count of values
    rows = [1, 2, 1, 0, 3, 1, 2, 3, 1, 2, 1]
    assert {12, 0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.count(rows)
  end

  test "sum" do
    # per-user row format = sum of values
    rows = [1, 2, -1, 0, 1, 2, -4, -2, 4, 1, -1, 2]
    assert {6.0, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.sum(rows)
  end

  test "avg" do
    # per-user row format = {sum of values, count of values}
    rows = [{1, 1}, {2, 1}, {-1, 1}, {0, 1}, {1, 1}, {2, 2}, {-4, 4}, {-2, 1}, {4, 3}, {-1, 2}]
    assert {0.3, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.avg(rows)
  end

  test "stddev" do
    # per-user row format = {sum of values, sum of squared values, count of values}
    rows = [{1, 1, 1}, {2, 4, 1}, {-1, 1, 1}, {0, 0, 1}, {1, 1, 1}, {2, 2, 2}, {-4, 4, 4}, {-2, 4, 1}, {4, 6, 3}]
    assert {0.8988882021697692, 0.0} = Anonymizer.new([MapSet.new()]) |> Anonymizer.stddev(rows)
  end

  test "min" do
    # per-user row format = collection of values
    rows = [[1, -2], [3], [4, 2, -3], [-2, 4], [-1], [-3, -2], [3], [4, -2, 1], [5], [-4], [-5, 4], [3], [-6, 3, 2]]
    assert -1 = Anonymizer.new([MapSet.new()]) |> Anonymizer.min(rows) |> round()
  end

  test "max" do
    # per-user row format = collection of values
    rows = [[1, -2], [3], [4, 2, -3], [-2, 4], [-1], [-3, -2], [3], [4, -2, 1], [5], [-4], [-5, 4], [3], [-6, 3, 2]]
    assert 3 = Anonymizer.new([MapSet.new()]) |> Anonymizer.max(rows) |> round()
  end

  test "median" do
    # per-user row format = collection of all values
    rows = [[1, 2], [3], [4, 2, -3], [2, 4], [0], [-3, -2], [3], [4, -2, 1], [5], [-4], [-5, 4], [3]]
    assert 1 = Anonymizer.new([MapSet.new()]) |> Anonymizer.median(rows) |> round()
  end
end
