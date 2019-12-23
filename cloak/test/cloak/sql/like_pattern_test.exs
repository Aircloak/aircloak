defmodule Cloak.Sql.LikePattern.Test do
  alias Cloak.Sql.{LikePattern, Expression}

  use ExUnit.Case, async: true

  test "unescapes trivial patterns",
    do: assert(%{value: "abcd"} = LikePattern.trivial_to_string(Expression.like_pattern("a~b~cd", "~")))

  describe "graphemes" do
    test "returns regular characters", do: assert(~w(a b c) = LikePattern.graphemes("abc", nil))

    test "returns escaped regular characters", do: assert(~w(a b c) = LikePattern.graphemes(~S[a\b\c]))

    test "returns escaped escape characters", do: assert(~w(a b ~ c) = LikePattern.graphemes("ab~~c", "~"))

    test "returns special characters", do: assert(["a", "b", :%, :_, "c"] = LikePattern.graphemes("ab%_c", nil))

    test "escapes special characters", do: assert(~w(a b % _ c) = LikePattern.graphemes("ab%%%_c", "%"))
  end

  describe "trivial?" do
    test "true when there are no special chars", do: assert(LikePattern.trivial?({"abc", nil, nil}))

    test "false when a % exists", do: refute(LikePattern.trivial?({"a%c", nil, nil}))

    test "false when a _ exists", do: refute(LikePattern.trivial?({"a_c", nil, nil}))
  end

  describe "simple?" do
    test "true when empty", do: assert(LikePattern.simple?({"", nil, nil}))

    test "true when trivial", do: assert(LikePattern.simple?({"abc", nil, nil}))

    test "false when a _ exists" do
      refute(LikePattern.simple?({"_abc", nil, nil}))
      refute(LikePattern.simple?({"abc_", nil, nil}))
    end

    test "true when the pattern starts or ends with a %" do
      assert LikePattern.simple?({"%abc", nil, nil})
      assert LikePattern.simple?({"abc%", nil, nil})
      assert LikePattern.simple?({"%abc%", nil, nil})
    end

    test "false when the pattern has a % in the middle", do: refute(LikePattern.simple?({"a%bc", nil, nil}))
  end

  describe "new" do
    test "does nothing for trivial patterns",
      do: assert({"abc", ~r/^abc$/usm, ~r/^abc$/usmi} = LikePattern.new("abc", nil))

    test "converts % to .*",
      do: assert({"a%c", ~r/^a.*c$/usm, ~r/^a.*c$/usmi} = LikePattern.new("a%c", nil))

    test "converts _ to .",
      do: assert({"a_c", ~r/^a.c$/usm, ~r/^a.c$/usmi} = LikePattern.new("a_c", nil))

    test "respects escapes",
      do: assert({"a\\%c", ~r/^a%c$/usm, ~r/^a%c$/usmi} = LikePattern.new("a~%c", "~"))

    test "escapes special regex chars",
      do: assert({".(*", ~r/^\.\(\*$/usm, ~r/^\.\(\*$/usmi} = LikePattern.new(".(*", nil))

    test "switches the escape character to \\", do: assert({~S[a\\b\%~c], _, _} = LikePattern.new(~S[a~\~b~%~~c], "~"))

    test "compresses multiple %%", do: assert({~S[%a%b%c%], _, _} = LikePattern.new(~S[%%a%%%b%c%%%%], nil))

    test "normalizes order of % and _",
      do: assert({~S[%__a%___bc%_], _, _} = LikePattern.new(~S[__%%a___%bc%_%%%], nil))

    test "normalizes complex like patterns",
      do: assert(LikePattern.new("a_%__%_b%c%%d___", nil) == LikePattern.new("a%____b%c%d___", nil))
  end
end
