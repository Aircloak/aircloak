defmodule Cloak.Sql.Compiler.ASTNormalization.Test do
  alias Cloak.Sql.{Compiler.ASTNormalization, Parser}

  use ExUnit.Case, async: true

  describe "rewriting distinct" do
    test "distinct without group by" do
      parsed = Parser.parse!("SELECT DISTINCT a, b, c FROM table")
      expected = Parser.parse!("SELECT a, b, c FROM table GROUP BY a, b, c")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "distinct with group by" do
      parsed = Parser.parse!("SELECT DISTINCT a, b + d, c FROM table GROUP BY b")
      expected = Parser.parse!("SELECT * FROM (SELECT a, b + d, c FROM table GROUP BY b) to_fix GROUP BY 1, 2, 3")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "distinct with aggregators"
  end
end
