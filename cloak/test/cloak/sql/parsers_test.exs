defmodule Cloak.Sql.Parsers.Test do
  use ExUnit.Case, async: true

  import Combine.Parsers.Text
  import Cloak.Sql.Parsers

  describe "sep_by1_eager" do
    test "a single item",
      do: assert([["a"]] = Combine.parse("a", sep_by1_eager(char("a"), char(","))))

    test "a single wrong item",
      do: assert({:error, _} = Combine.parse("b", sep_by1_eager(char("a"), char(","))))

    test "multiple items",
      do:
        assert(
          [["a", ",", "a", ",", "a"]] =
            Combine.parse("a,a,a", sep_by1_eager(char("a"), char(",")))
        )

    test "multiple items with a wrong one",
      do: assert({:error, _} = Combine.parse("a,b,a", sep_by1_eager(char("a"), char(","))))
  end

  describe "choice_deepest_error" do
    test "success - the first clause matches",
      do:
        assert(
          ["a"] = Combine.parse("a", choice_deepest_error([char("a"), char("b") |> char("b")]))
        )

    test "success - the second clause matches",
      do:
        assert(
          ["b", "b"] =
            Combine.parse("bb", choice_deepest_error([char("a"), char("b") |> char("b")]))
        )

    test "failure - the second clause matches more" do
      assert {:error, error} =
               Combine.parse("bc", choice_deepest_error([char("a"), char("b") |> char("b")]))

      assert error =~ ~r/Expected `b`, but found `c`/
    end

    test "failure - the first clause matches more" do
      assert {:error, error} =
               Combine.parse("bc", choice_deepest_error([char("b") |> char("b"), char("a")]))

      assert error =~ ~r/Expected `b`, but found `c`/
    end
  end

  describe "switch" do
    test "reports the deeper error if all options fail" do
      assert {:error, error} =
               Combine.parse(
                 "abc",
                 switch([
                   {char("a") |> char("a"), noop()},
                   {char("a") |> char("b") |> char("b"), noop()}
                 ])
               )

      assert error =~ ~r/Expected `b`, but found `c`/
    end
  end

  describe "return" do
    test "works outside of a pipeline",
      do: assert([:value] = Combine.parse("abc", return(:value)))

    test "works in a pipeline",
      do:
        assert(
          [:value] =
            Combine.parse("abc", Combine.Parsers.Base.ignore(char("a")) |> return(:value))
        )
  end
end
