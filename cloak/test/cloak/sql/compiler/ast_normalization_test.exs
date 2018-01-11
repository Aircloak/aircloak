defmodule Cloak.Sql.Compiler.ASTNormalization.Test do
  alias Cloak.Sql.{Compiler.ASTNormalization, Parser}

  use ExUnit.Case, async: true

  describe "rewriting distinct" do
    test "does not rewrite the top-level query" do
      parsed = Parser.parse!("SELECT DISTINCT a, b, c FROM table")
      assert ASTNormalization.normalize(parsed) == parsed
    end

    test "distinct without group by" do
      parsed = Parser.parse!("SELECT COUNT(*) FROM (SELECT DISTINCT a, b, c FROM table) x")
      expected = Parser.parse!("SELECT COUNT(*) FROM (SELECT a, b, c FROM table GROUP BY 1, 2, 3) x")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "distinct with group by" do
      normalized = Parser.parse!("""
        SELECT COUNT(*) FROM (
          SELECT DISTINCT a, b + d, c FROM table GROUP BY b
        ) x
      """) |> ASTNormalization.normalize()
      expected = Parser.parse!("""
        SELECT COUNT(*) FROM (
          SELECT * FROM (
            SELECT a, b + d, c FROM table GROUP BY b
          ) __ac_distinct GROUP BY 1, 2, 3
        ) x
      """)

      assert normalized == expected
    end

    test "distinct with aggregators" do
      parsed = Parser.parse!("SELECT COUNT(*) FROM (SELECT DISTINCT COUNT(*) + 1, ABS(AVG(a)) FROM table) x")
      expected = Parser.parse!("SELECT COUNT(*) FROM (SELECT COUNT(*) + 1, ABS(AVG(a)) FROM table) x")

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
      expected = Parser.parse!("SELECT * FROM table WHERE x <> 1 AND (x <> 2 AND x <> 3)")

      assert ASTNormalization.normalize(parsed) == expected
    end

    test "acts on subqueries" do
      parsed = Parser.parse!("SELECT * FROM (SELECT * FROM Table WHERE x NOT IN (1, 2, 3)) x")
      expected = Parser.parse!("SELECT * FROM (SELECT * FROM Table WHERE x <> 1 AND (x <> 2 AND x <> 3)) x")

      assert ASTNormalization.normalize(parsed) == expected
    end
  end

  describe "rewriting NOT" do
    %{
      "=" => "<>",
      "<" => ">=",
      ">" => "<=",
      "LIKE" => "NOT LIKE",
      "ILIKE" => "NOT ILIKE",
    }
    |> Enum.each(fn({op1, op2}) ->
      test "transforms #{op1} into #{op2}" do
        parsed = Parser.parse!("SELECT * FROM table WHERE NOT x #{unquote(op1)} 'foo'")
        expected = Parser.parse!("SELECT * FROM table WHERE x #{unquote(op2)} 'foo'")

        assert ASTNormalization.normalize(parsed) == expected
      end

      test "transforms #{op2} into #{op1}" do
        parsed = Parser.parse!("SELECT * FROM table WHERE NOT x #{unquote(op2)} 'foo'")
        expected = Parser.parse!("SELECT * FROM table WHERE x #{unquote(op1)} 'foo'")

        assert ASTNormalization.normalize(parsed) == expected
      end
    end)

    test "multiple NOTs" do
      parsed = Parser.parse!("SELECT * FROM table WHERE NOT NOT NOT NOT NOT x = y")
      expected = Parser.parse!("SELECT * FROM table WHERE x <> y")

      assert ASTNormalization.normalize(parsed) == expected
    end
  end
end
