defmodule Compliance.QueryFeatures.Test do
  use ComplianceCase, async: true

  @moduletag :query_features

  Enum.each(table_uids(), fn({table, uid}) ->
    @tag compliance: "offset and limit with order by constant on #{table}"
    test "offset and limit with order by contant on #{table}", context do
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
    test "offset and limit with order by contant on subquery to #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*) FROM (
          SELECT #{unquote(uid)}, 'a constant' AS const
          FROM #{unquote(table)}
          ORDER BY 2
          LIMIT 10
          OFFSET 10
        ) x
      """)
    end
  end)
end
