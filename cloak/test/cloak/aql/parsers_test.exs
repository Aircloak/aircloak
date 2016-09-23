defmodule Cloak.Aql.Parsers.Test do
  use ExUnit.Case, async: true

  import Combine.Parsers.Text
  import Cloak.Aql.Parsers

  describe "sep_by1_failing" do
    test "a single item" do
      assert [["a"]] = Combine.parse("a", sep_by1_failing(char("a"), char(",")))
    end

    test "a single wrong item" do
      assert {:error, _} = Combine.parse("b", sep_by1_failing(char("a"), char(",")))
    end

    test "multiple items" do
      assert [["a", "a", "a"]] = Combine.parse("a,a,a", sep_by1_failing(char("a"), char(",")))
    end

    test "multiple items with a wrong one" do
      assert {:error, _} = Combine.parse("a,b,a", sep_by1_failing(char("a"), char(",")))
    end
  end
end
