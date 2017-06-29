defmodule Cloak.Sql.LikePattern.Test do
  alias Cloak.Sql.LikePattern

  use ExUnit.Case, async: true

  describe "graphemes" do
    test "returns regular characters", do:
      assert ~w(a b c) = LikePattern.graphemes({"abc", nil})

    test "returns escaped regular characters", do:
      assert ~w(a b c) = LikePattern.graphemes({~S[a\b\c], "\\"})

    test "returns escaped escape characters"

    test "returns special characters"

    test "doesn't escape when no escape given"
  end
end
