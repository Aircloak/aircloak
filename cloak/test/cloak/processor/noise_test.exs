defmodule Cloak.Processor.Noise.Test do
  use ExUnit.Case, async: true
  use ExCheck

  alias Cloak.Processor.Noise

  property "the random seed is the same for reordered lists" do
    for_all x in list(int) do
      Noise.make_seed(MapSet.new(x)) == Noise.make_seed(MapSet.new(Enum.shuffle(x)))
    end
  end
end
