defmodule Cloak.Processor.Noise.Test do
  use ExUnit.Case, async: true
  use ExCheck

  alias Cloak.Processor.Noise

  property "the random seed is the same for reordered lists" do
    for_all x in list(int) do
      Noise.random_seed(x) == Noise.random_seed(Enum.shuffle(x))
    end
  end
end
