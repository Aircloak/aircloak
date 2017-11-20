defmodule Aircloak.OxfordComma.Test do
  use ExUnit.Case, async: true

  alias Aircloak.OxfordComma

  test "a single entity is unchanged", do:
    assert OxfordComma.join(["first"]) == "first"

  test "two parts don't get a comma", do:
    assert OxfordComma.join(["first", "second"]) == "first and second"

  test "three parts don't get a comma", do:
    assert OxfordComma.join(["first", "second", "third"]) == "first, second, and third"
end
