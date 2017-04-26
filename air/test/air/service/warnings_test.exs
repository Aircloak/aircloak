defmodule Air.Service.WarningsTest do
  use Air.SchemaCase, async: true

  alias Air.Service.Warnings

  describe "known_problems?" do
    test "there are no known problems as of yet", do:
      refute Problems.known_problems?()
  end
end
