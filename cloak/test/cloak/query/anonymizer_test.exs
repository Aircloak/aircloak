defmodule Cloak.Query.AnonimyzerTest do
  use ExUnit.Case, async: true

  alias Cloak.Query.Anonymizer

  test "sufficiently_large?" do
    assert {true, _} = Anonymizer.new([]) |> Anonymizer.sufficiently_large?(20)
    assert {false, _} = Anonymizer.new([]) |> Anonymizer.sufficiently_large?(2)
  end

  test "count" do
    rows = [1, 2, 1, 0, 3, 1, 2, 3, 1, 2, 1]
    assert {12, 0} = Anonymizer.new([]) |> Anonymizer.count(rows)
  end

  test "sum" do
    rows = [1, 2, -1, 0, 1, 2, -4, -2, 4, 1, -1, 2]
    assert {6, 0} = Anonymizer.new([]) |> Anonymizer.sum(rows)
  end

  test "avg" do
    rows = [{1, 1}, {2, 1}, {-1, 1}, {0, 1}, {1, 1}, {2, 2}, {-4, 4}, {-2, 1}, {4, 3}, {-1, 2}]
    assert {0.3, 0.0} = Anonymizer.new([]) |> Anonymizer.avg(rows)
  end

  test "stddev" do
    rows = [{1, 1, 1}, {2, 4, 1}, {-1, 1, 1}, {0, 0, 1}, {1, 1, 1}, {2, 2, 2}, {-4, 4, 4}, {-2, 4, 1}, {4, 6, 3}]
    assert {0.8988882021697692, 0.0} = Anonymizer.new([]) |> Anonymizer.stddev(rows)
  end

  test "min" do
    rows = [[1, -2], [3], [4, 2, -3], [-2, 4], [-1], [-3, -2], [3], [4, -2, 1], [5], [-4], [-5, 4], [3], [-6, 3, 2]]
    assert -1 = Anonymizer.new([]) |> Anonymizer.min(rows)
  end

  test "max" do
    rows = [[1, -2], [3], [4, 2, -3], [-2, 4], [-1], [-3, -2], [3], [4, -2, 1], [5], [-4], [-5, 4], [3], [-6, 3, 2]]
    assert 3 = Anonymizer.new([]) |> Anonymizer.max(rows)
  end

  test "median" do
    rows = [[1, 2], [3], [4, 2, -3], [2, 4], [0], [-3, -2], [3], [4, -2, 1], [5], [-4], [-5, 4], [3]]
    assert 2 = Anonymizer.new([]) |> Anonymizer.max(rows)
  end
end
