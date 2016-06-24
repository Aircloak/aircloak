defmodule Cloak.Processor.Anonymizer.Test do
  use ExUnit.Case, async: true
  use ExCheck

  alias Cloak.Processor.Anonymizer

  property "the random seed is the same for reordered lists" do
    for_all x in list(int) do
      Anonymizer.new(MapSet.new(x)) == Anonymizer.new(MapSet.new(Enum.shuffle(x)))
    end
  end
end
