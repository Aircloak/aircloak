defmodule Cloak.Sql.Compiler.ASTNormalization.Test do
  alias Cloak.Sql.{Compiler.ASTNormalization, Parser}
  alias Cloak.Test.QueryHelpers

  use ExUnit.Case, async: true

  defmacrop assert_equivalent(to_normalize, expected) do
    quote bind_quoted: [to_normalize: to_normalize, expected: expected] do
      parsed = to_normalize |> Parser.parse!() |> ASTNormalization.normalize() |> QueryHelpers.scrub_locations()
      expected = expected |> Parser.parse!() |> QueryHelpers.scrub_locations()

      assert parsed == expected
    end
  end

  describe "rewriting distinct" do
    test "does not rewrite the top-level query" do
      parsed = Parser.parse!("SELECT DISTINCT a, b, c FROM table")
      assert ASTNormalization.normalize(parsed) == parsed
    end

    test "distinct without group by", do:
      assert_equivalent(
        "SELECT COUNT(*) FROM (SELECT DISTINCT a, b, c FROM table) x",
        "SELECT COUNT(*) FROM (SELECT a, b, c FROM table GROUP BY 1, 2, 3) x"
      )

    test "distinct with group by", do:
      assert_equivalent(
        """
          SELECT COUNT(*) FROM (
            SELECT DISTINCT a, b + d, c FROM table GROUP BY b
          ) x
        """,
        """
          SELECT COUNT(*) FROM (
            SELECT * FROM (
              SELECT a, b + d, c FROM table GROUP BY b
            ) __ac_distinct GROUP BY 1, 2, 3
          ) x
      """)

    test "distinct with aggregators", do:
      assert_equivalent(
        "SELECT COUNT(*) FROM (SELECT DISTINCT COUNT(*) + 1, ABS(AVG(a)) FROM table) x",
        "SELECT COUNT(*) FROM (SELECT COUNT(*) + 1, ABS(AVG(a)) FROM table) x"
      )

    test "rewrites subqueries in joins", do:
      assert_equivalent(
        "SELECT * FROM foo JOIN (SELECT DISTINCT COUNT(*) + 1, ABS(AVG(a)) FROM table) x ON a = b",
        "SELECT * FROM foo JOIN (SELECT COUNT(*) + 1, ABS(AVG(a)) FROM table) x ON a = b"
      )
  end

  describe "rewriting NOT IN" do
    test "with one element in LHS", do:
      assert_equivalent(
        "SELECT * FROM table WHERE x NOT IN ('string')",
        "SELECT * FROM table WHERE x <> 'string'"
      )

    test "with many elements in LHS", do:
      assert_equivalent(
        "SELECT * FROM table WHERE x NOT IN (1, 2, 3)",
        "SELECT * FROM table WHERE x <> 1 AND (x <> 2 AND x <> 3)"
      )

    test "acts on subqueries", do:
      assert_equivalent(
        "SELECT * FROM (SELECT * FROM Table WHERE x NOT IN (1, 2, 3)) x",
        "SELECT * FROM (SELECT * FROM Table WHERE x <> 1 AND (x <> 2 AND x <> 3)) x"
      )
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
      test "transforms #{op1} into #{op2}", do:
        assert_equivalent(
          "SELECT * FROM table WHERE NOT x #{unquote(op1)} 'foo'",
          "SELECT * FROM table WHERE x #{unquote(op2)} 'foo'"
        )

      test "transforms #{op2} into #{op1}", do:
        assert_equivalent(
          "SELECT * FROM table WHERE NOT x #{unquote(op2)} 'foo'",
          "SELECT * FROM table WHERE x #{unquote(op1)} 'foo'"
        )
    end)

    test "multiple NOTs", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT NOT NOT NOT NOT x = y",
        "SELECT * FROM table WHERE x <> y"
      )

    test "NOT x IS NULL is equivalent to x IS NOT NULL", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT x IS NULL",
        "SELECT * FROM table WHERE x IS NOT NULL"
      )

    test "NOT x IS NOT NULL is equivalent to x IS NULL", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT x IS NOT NULL",
        "SELECT * FROM table WHERE x IS NULL"
      )

    test "NOT x IN (single_value)", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT x IN (1)",
        "SELECT * FROM table WHERE x <> 1"
      )

    test "NOT x IN (multiple, values)", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT x IN (1, 2, 3)",
        "SELECT * FROM table WHERE x <> 1 AND (x <> 2 AND x <> 3)"
      )

    test "NOT x NOT IN (single_value)", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT x NOT IN (1)",
        "SELECT * FROM table WHERE x = 1"
      )

    test "NOT x NOT IN (multiple, values)", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT x NOT IN (1, 2, 3)",
        "SELECT * FROM table WHERE x IN (1, 2, 3)"
      )

    test "BETWEEN", do:
      assert_equivalent(
        "SELECT * FROM table WHERE NOT x BETWEEN 1 AND 2",
        "SELECT * FROM table WHERE x < 1 OR x >= 2"
      )

    test "applied to subqueries", do:
      assert_equivalent(
        "SELECT * FROM (SELECT * FROM table WHERE NOT x = 1) x",
        "SELECT * FROM (SELECT * FROM table WHERE x <> 1) x"
      )

    test "applied in HAVING", do:
      assert_equivalent(
        "SELECT COUNT(*) FROM table HAVING NOT x = 1",
        "SELECT COUNT(*) FROM table HAVING x <> 1"
      )

    test "applied in ON", do:
      assert_equivalent(
        "SELECT COUNT(*) FROM table JOIN table2 ON NOT x = y",
        "SELECT COUNT(*) FROM table JOIN table2 ON x <> y"
      )

    test "negation of conjunction", do:
      assert_equivalent(
        "SELECT COUNT(*) FROM table WHERE NOT (x = 1 AND y = 2)",
        "SELECT COUNT(*) FROM table WHERE x <> 1 OR y <> 2"
      )

    test "negation of disjunction", do:
      assert_equivalent(
        "SELECT COUNT(*) FROM table WHERE NOT (x = 1 OR y = 2)",
        "SELECT COUNT(*) FROM table WHERE x <> 1 AND y <> 2"
      )

    test "negation of complex boolean expression", do:
      assert_equivalent(
        "SELECT COUNT(*) FROM table WHERE NOT (x = 1 AND (y = 2 AND NOT z = 3))",
        "SELECT COUNT(*) FROM table WHERE x <> 1 OR (y <> 2 OR z = 3)"
      )
  end

  test "normalizing IN(single_value)", do:
    assert_equivalent(
      "SELECT * FROM table WHERE string IN ('a')",
      "SELECT * FROM table WHERE string = 'a'"
    )

  test "normalizing date_trunc's first argument", do:
    assert_equivalent(
      "SELECT date_trunc('YEAR', column) FROM table",
      "SELECT date_trunc(lower('YEAR'), column) FROM table"
    )

  %{
    "ceiling" => "ceil",
    "lcase" => "lower",
    "ucase" => "upper",
    "dow" => "weekday",
  }
  |> Enum.each(fn({synonym, function}) ->
    test "#{synonym} is a synonym for #{function}", do:
      assert %{columns: [{:function, %{canonical_name: unquote(function), synonym_used: unquote(synonym)}, _, _}]} =
        Parser.parse!("SELECT #{unquote(synonym)}(column) FROM table") |> ASTNormalization.normalize()
  end)

  %{
    "mod" => "%",
    "pow" => "^",
  }
  |> Enum.each(fn({synonym, operator}) ->
    test "#{synonym} is a synonym for #{operator}", do:
      assert %{columns: [{:function, %{canonical_name: unquote(operator), synonym_used: unquote(synonym)}, _, _}]} =
        Parser.parse!("SELECT #{unquote(synonym)}(column, 10) FROM table") |> ASTNormalization.normalize()
  end)

  test "dow is a synonym for weekday in extract", do:
    assert %{columns: [{:function, %{canonical_name: "weekday", synonym_used: "dow"}, _, _}]} =
      Parser.parse!("SELECT EXTRACT(dow FROM column) FROM table") |> ASTNormalization.normalize()
end
