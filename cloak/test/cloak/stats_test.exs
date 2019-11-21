defmodule Cloak.Stats.Test do
  use ExUnit.Case, async: true
  alias Cloak.Stats

  test "mean" do
    assert 0..1000 |> Enum.shuffle() |> Stats.mean() == 500.0
  end

  test "stddev" do
    assert 0..1000 |> Enum.shuffle() |> Stats.stddev() |> round() == 289
  end

  test "variance" do
    assert 0..1000 |> Enum.shuffle() |> Stats.variance() |> :math.sqrt() |> round() == 289
  end

  test "sum" do
    assert 0..1000 |> Enum.shuffle() |> Stats.sum() == 500_500
  end
end
