defmodule Cloak.Sql.LikePattern.Test do
  alias Cloak.Sql.{LikePattern, Expression}

  use ExUnit.Case, async: true

  describe "trivial_to_string" do
    test "fails for non-trivial patterns", do:
      assert_raise MatchError, fn() ->
        LikePattern.trivial_to_string(Expression.constant(:like_pattern, {"a%b", nil}))
      end

    test "unescapes trivial patterns", do:
      assert %{value: "abcd"} = LikePattern.trivial_to_string(Expression.constant(:like_pattern, {"a~b~cd", "~"}))
  end

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

  describe "lowercase_pattern" do
    test "produces lowercase patterns for trivial patterns", do:
      assert Expression.like_pattern("abc", "\\") ==
        Expression.like_pattern("AbC", "\\") |> LikePattern.lowercase_pattern()

    test "produces lowercase patterns for complex patterns", do:
      assert Expression.like_pattern("a%b_c", "\\") ==
        Expression.like_pattern("A%b_C", "\\") |> LikePattern.lowercase_pattern()

    test "fails if a pattern hasn't been normalised", do:
      assert_raise RuntimeError, fn() ->
        LikePattern.lowercase_pattern(Expression.like_pattern("a%b", nil))
      end
  end

  describe "to_regex" do
    test "converts % to .*", do:
      assert ~r/^a.*c$/ = LikePattern.to_regex({"a%c", nil})

    test "converts _ to .", do:
      assert ~r/^a.c$/ = LikePattern.to_regex({"a_c", nil})

    test "respects escapes", do:
      assert ~r/^a%c$/ = LikePattern.to_regex({"a~%c", "~"})

    test "escapes special regex chars", do:
      assert ~r/^\.\(\*$/ = LikePattern.to_regex({".(*", nil})

    test "passes options to regex compilation", do:
      assert ~r/^abc$/i = LikePattern.to_regex({"abc", nil}, "i")
  end
end
