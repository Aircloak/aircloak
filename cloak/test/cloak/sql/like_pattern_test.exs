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

  describe "trivial?" do
    test "true when there are no special chars", do:
      assert LikePattern.trivial?({"abc", nil})

    test "false when a % exists", do:
      refute LikePattern.trivial?({"a%c", nil})

    test "false when a _ exists", do:
      refute LikePattern.trivial?({"a_c", nil})

    test "true when special characters are escaped", do:
      assert LikePattern.trivial?({"a~%~_c", "~"})
  end

  describe "normalize" do
    test "does nothing for trivial patterns", do:
      assert {"abc", "\\"} = LikePattern.normalize({"abc", nil})

    test "switches the escape character to \\", do:
      assert {~S[a\\b\%~c], "\\"} = LikePattern.normalize({~S[a~\~b~%~~c], "~"})

    test "compresses multiple %%", do:
      assert {~S[%a%b%c%], "\\"} = LikePattern.normalize({~S[%%a%%%b%c%%%%], nil})

    test "normalizes order of % and _", do:
      assert {~S[%__a%___bc%_], "\\"} = LikePattern.normalize({~S[__%%a___%bc%_%%%], nil})
  end
end
