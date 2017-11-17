defmodule Cloak.Sql.Compiler.ASTNormalization.Test do
  alias Cloak.Sql.{Compiler.ASTNormalization, Parser, Query}

  use ExUnit.Case, async: true

  describe "rewriting distinct" do
    test "distinct without group by" do
      parsed = Parser.parse!("SELECT DISTINCT a, b, c FROM table")
      expected = Parser.parse!("SELECT a, b, c FROM table GROUP BY 1, 2, 3")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "distinct with group by" do
      normalized = Parser.parse!("SELECT DISTINCT a, b + d, c FROM table GROUP BY b")
        |> ASTNormalization.normalize()
        |> scrub_subquery_aliases()
      expected = Parser.parse!("SELECT * FROM (SELECT a, b + d, c FROM table GROUP BY b) alias GROUP BY 1, 2, 3")
        |> scrub_subquery_aliases()

      assert normalized == expected
    end

    test "distinct with aggregators" do
      parsed = Parser.parse!("SELECT DISTINCT COUNT(*) + 1, ABS(AVG(a)) FROM table")
      expected = Parser.parse!("SELECT COUNT(*) + 1, ABS(AVG(a)) FROM table")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "rewrites subqueries" do
      parsed = Parser.parse!("SELECT * FROM (SELECT DISTINCT COUNT(*) + 1, ABS(AVG(a)) FROM table) x")
      expected = Parser.parse!("SELECT * FROM (SELECT COUNT(*) + 1, ABS(AVG(a)) FROM table) x")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "rewrites subqueries in joins" do
      parsed = Parser.parse!("SELECT * FROM foo JOIN (SELECT DISTINCT COUNT(*) + 1, ABS(AVG(a)) FROM table) x ON a = b")
      expected = Parser.parse!("SELECT * FROM foo JOIN (SELECT COUNT(*) + 1, ABS(AVG(a)) FROM table) x ON a = b")

      assert ASTNormalization.normalize(parsed) == expected
    end
  end

  describe "rewriting NOT IN" do
    test "with one element in LHS" do
      parsed = Parser.parse!("SELECT * FROM table WHERE x NOT IN ('string')")
      expected = Parser.parse!("SELECT * FROM table WHERE x <> 'string'")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "with many elements in LHS" do
      parsed = Parser.parse!("SELECT * FROM table WHERE x NOT IN (1, 2, 3)")
      expected = Parser.parse!("SELECT * FROM table WHERE x <> 1 AND x <> 2 AND x <> 3")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "acts on subqueries" do
      parsed = Parser.parse!("SELECT * FROM (SELECT * FROM Table WHERE x NOT IN (1, 2, 3)) x")
      expected = Parser.parse!("SELECT * FROM (SELECT * FROM Table WHERE x <> 1 AND x <> 2 AND x <> 3) x")

      assert ASTNormalization.normalize(parsed) == expected
    end
  end

  defp scrub_subquery_aliases(query), do:
    put_in(query, [Query.Lenses.subqueries() |> Lens.key(:alias)], nil)
end
