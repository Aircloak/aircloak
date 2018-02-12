defmodule Compliance.QueryFeatures.Test do
  use ComplianceCase, async: true

  @moduletag :query_features

  Enum.each(table_uids(), fn({table, uid}) ->
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
          SELECT #{unquote(uid)}, 'a constant'
          FROM #{unquote(table)}
          ORDER BY 2
          LIMIT 10
          OFFSET 10
        ) x
      """)
    end
  end)

  Enum.each(nullable_columns(), fn({column, table, uid}) ->
    for direction <- ["ASC", "DESC", ""], nulls <- ["NULLS FIRST", "NULLS LAST", ""] do
      @tag compliance: "order by #{direction} #{nulls} on #{column} in #{table}"
      test "order by #{direction} #{nulls} on #{column} in #{table}", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT BUCKET(#{unquote(column)} BY 10)
          FROM #{unquote(table)}
          ORDER BY 1 #{unquote(direction)} #{unquote(nulls)}
        """)
      end

      @tag compliance: "order by #{direction} #{nulls} on #{column} in #{table} subquery"
      test "order by #{direction} #{nulls} on #{column} in #{table} subquery", context do
        context
        |> assert_consistent_and_not_failing("""
          SELECT foo FROM (
            SELECT #{unquote(uid)}, BUCKET(#{unquote(column)} BY 10) AS foo
            FROM #{unquote(table)}
            ORDER BY 1 #{unquote(direction)} #{unquote(nulls)}
            LIMIT 10
          ) x
        """)
      end
    end
  end)
end
