defmodule Compliance.WhereClauseFilters.Text do
  use ComplianceCase, async: true, timeout: :timer.minutes(2)

  Enum.each(numerical_columns() |> raw_columns(), fn({column, table, uid}) ->
    @tag compliance: "#{column} #{table} WHERE-clause equality in subquery"
    test "input #{column} with a WHERE-clause equality in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} = 10
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause inequality in subquery"
    test "on input #{column} with a WHERE-clause inequality in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} <> 10
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause IN in subquery"
    test "input #{column} with a WHERE-clause IN in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} IN (1, 2, 3)
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause NOT IN in subquery"
    test "input #{column} with a WHERE-clause NOT IN in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} NOT IN (1, 2, 3)
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause equality"
    test "input #{column} with a WHERE-clause equality on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} = 10
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause inequality"
    test "on input #{column} with a WHERE-clause inequality on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} <> 10
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause IN"
    test "input #{column} with a WHERE-clause IN on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} IN (1, 2, 3)
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause NOT IN"
    test "input #{column} with a WHERE-clause NOT IN on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} NOT IN (1, 2, 3)
      """)
    end
  end)

  Enum.each(text_columns(), fn({column, table, uid}) ->
    @tag compliance: "#{column} #{table} WHERE-clause equality in subquery"
    test "input #{column} with a WHERE-clause equality in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} = 'Emma'
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause inequality in subquery"
    test "on input #{column} with a WHERE-clause inequality in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} <> 'Emma'
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause LIKE in subquery"
    test "input #{column} with a WHERE-clause LIKE in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} LIKE '%Em%'
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause ILIKE in subquery"
    test "input #{column} with a WHERE-clause ILIKE in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} ILIKE '%em%'
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause IN in subquery"
    test "input #{column} with a WHERE-clause IN in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} IN ('Jan', 'Herman', 'Berlin')
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause NOT IN in subquery"
    test "input #{column} with a WHERE-clause NOT IN in a sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT
          count(*)
        FROM (
          SELECT #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE #{unquote(column)} NOT IN ('Jan', 'Herman', 'Berlin')
          GROUP BY 1
        ) table_alias
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause equality"
    test "input #{column} with a WHERE-clause equality on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} = 'Emma'
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause inequality"
    test "on input #{column} with a WHERE-clause inequality on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} <> 'Emma'
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause LIKE"
    test "input #{column} with a WHERE-clause LIKE on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} LIKE '%Em%'
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause ILIKE"
    test "input #{column} with a WHERE-clause ILIKE on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} ILIKE '%em%'
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause IN"
    test "input #{column} with a WHERE-clause IN on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} IN ('Jan', 'Herman', 'Berlin')
      """)
    end

    @tag compliance: "#{column} #{table} WHERE-clause NOT IN"
    test "input #{column} with a WHERE-clause NOT IN on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT count(*)
        FROM #{unquote(table)}
        WHERE #{unquote(column)} NOT IN ('Jan', 'Herman', 'Berlin')
      """)
    end
  end)
end
