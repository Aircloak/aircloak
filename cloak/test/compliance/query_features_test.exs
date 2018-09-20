defmodule Compliance.QueryFeatures.Test do
  use ComplianceCase, async: true

  @moduletag :query_features

  @tag compliance: "multiple join"
  test "multiple join", context do
    [{table1, uid1}, {table2, uid2}, {table3, uid3} | _] = table_uids()

    context
    |> assert_consistent_and_not_failing("""
      SELECT COUNT(*)
      FROM #{table1} INNER JOIN #{table2}
      ON #{table1}.#{uid1} = #{table2}.#{uid2}
      INNER JOIN #{table3}
      ON #{table2}.#{uid2} = #{table3}.#{uid3}
    """)
  end

  @tag compliance: "multiple join in subquery"
  test "multiple join in subquery", context do
    [{table1, uid1}, {table2, uid2}, {table3, uid3} | _] = table_uids()

    context
    |> assert_consistent_and_not_failing("""
      SELECT COUNT(*) FROM (
        SELECT #{table1}.#{uid1}
        FROM #{table1} INNER JOIN #{table2}
        ON #{table1}.#{uid1} = #{table2}.#{uid2}
        INNER JOIN #{table3}
        ON #{table2}.#{uid2} = #{table3}.#{uid3}
      ) foo
    """)
  end

  Enum.each(table_uids(), fn {table, uid} ->
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

  Enum.each(nullable_columns(), fn {column, table, uid} ->
    for direction <- ["ASC", "DESC", ""],
        nulls <- ["NULLS FIRST", "NULLS LAST", ""] do
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
            SELECT #{unquote(uid)}, BUCKET(#{unquote(column)} BY 0.1) AS foo
            FROM #{unquote(table)}
            ORDER BY 2 #{unquote(direction)} #{unquote(nulls)}
            LIMIT 10
          ) x
        """)
      end
    end
  end)
end
