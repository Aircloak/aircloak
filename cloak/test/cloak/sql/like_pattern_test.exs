defmodule Cloak.Sql.LikePattern.Test do
  alias Cloak.Sql.LikePattern

  use ExUnit.Case, async: true

  describe "graphemes" do
    test "returns regular characters", do:
      assert ~w(a b c) = LikePattern.graphemes({"abc", nil})

    test "returns escaped regular characters", do:
      assert ~w(a b c) = LikePattern.graphemes({~S[a\b\c], "\\"})

    test "returns escaped escape characters", do:
      assert ~w(a b ~ c) = LikePattern.graphemes({"ab~~c", "~"})

    test "returns special characters", do:
      assert ["a", "b", :%, :_, "c"] = LikePattern.graphemes({"ab%_c", nil})

    test "escapes special characters", do:
      assert ~w(a b % _ c) = LikePattern.graphemes({"ab%%%_c", "%"})
  end
end
