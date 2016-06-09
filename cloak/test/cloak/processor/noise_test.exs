defmodule Cloak.Processor.Noise.Test do
  use ExUnit.Case, async: true

  alias Cloak.Processor.Noise

  test "the random seed is the same for reordered lists" do
    assert Noise.random_seed([1, 2, 3]) == Noise.random_seed([2, 3, 1])
  end

  test "the random seed is the same for lists with the same unique elements" do
    assert Noise.random_seed([1, 2, 3]) == Noise.random_seed([1, 1, 2, 2, 3, 3])
  end
end
