defmodule Compliance.QueryFeatures.Test do
  use ComplianceCase, async: true

  @moduletag :query_features

  Enum.each(table_uids(), fn table ->
    @tag compliance: "offset and limit with order by constant on #{table}"
    test "offset and limit with order by constant on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT 'a constant', COUNT(*)
        FROM #{unquote(table)}
        ORDER BY 1
        LIMIT 10
        OFFSET 10
      """)
    end

    @tag compliance: "offset and limit with order by constant on subquery to #{table}"
    test "offset and limit with order by constant on subquery to #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*) FROM (
          SELECT user_id, 'a constant'
          FROM #{unquote(table)}
          ORDER BY 2 ASC NULLS FIRST
          LIMIT 10
          OFFSET 10
        ) x
      """)
    end

    @tag compliance: "limit only with order by constant on subquery to #{table}"
    test "limit only with order by constant on subquery to #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*) FROM (
          SELECT user_id, 'a constant'
          FROM #{unquote(table)}
          ORDER BY 1, 2 DESC NULLS FIRST
          LIMIT 10
        ) x
      """)
    end
  end)

  Enum.each(nullable_columns(), fn {column, table} ->
    for direction <- ["ASC", "DESC", ""],
        nulls <- ["NULLS FIRST", "NULLS LAST"] do
      @tag compliance: "order by #{direction} #{nulls} on #{column} in #{table}"
      test "order by #{direction} #{nulls} on #{column} in #{table}", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT BUCKET(#{unquote(column)} BY 0.1)
          FROM #{unquote(table)}
          ORDER BY 1 #{unquote(direction)} #{unquote(nulls)}
        """)
      end
    end

    for direction <- ["ASC", "DESC", ""],
        nulls <- ["NULLS FIRST", "NULLS LAST"] do
      @tag compliance: "order by #{direction} #{nulls} on #{column} in #{table} subquery"
      test "order by #{direction} #{nulls} on #{column} in #{table} subquery", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT foo FROM (
            SELECT user_id, BUCKET(#{unquote(column)} BY 0.1) AS foo
            FROM #{unquote(table)}
            ORDER BY 1, 2 #{unquote(direction)} #{unquote(nulls)}
            LIMIT 10
          ) x
        """)
      end
    end
  end)
end
