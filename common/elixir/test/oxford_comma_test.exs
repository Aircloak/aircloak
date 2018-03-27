defmodule Aircloak.OxfordComma.Test do
  use ExUnit.Case, async: true

  alias Aircloak.OxfordComma

  test "an empty list becomes an empty string", do: assert(OxfordComma.join([]) == "")

  test "a single entity is unchanged", do: assert(OxfordComma.join(["first"]) == "first")

  test "two parts don't get a comma",
    do: assert(OxfordComma.join(["first", "second"]) == "first and second")

  test "three parts get a comma before the and",
    do: assert(OxfordComma.join(["first", "second", "third"]) == "first, second, and third")
end
