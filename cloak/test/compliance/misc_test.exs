defmodule Compliance.MiscTest do
  use ComplianceCase, async: true

  @moduletag :works

  Enum.each(table_uids(), fn {table, uid} ->
    @tag compliance: "sample users from #{table} in query"
    test "sample users from #{table} test in query", context do
      context
      # Oracle uses a different sample algorithm than other databases, so it's not included in these tests.
      |> disable_for(Cloak.DataSource.Oracle)
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*), COUNT(DISTINCT #{unquote(uid)}) FROM #{unquote(table)} SAMPLE_USERS 25%
      """)
    end

    @tag compliance: "sample users from #{table} in subquery"
    test "sample users from #{table} test in subquery", context do
      context
      # Oracle uses a different sample algorithm than other databases, so it's not included in these tests.
      |> disable_for(Cloak.DataSource.Oracle)
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*) FROM (SELECT DISTINCT #{unquote(uid)} FROM #{unquote(table)} SAMPLE_USERS 25%) AS t
      """)
    end
  end)

  Enum.each(numerical_columns(), fn {column, table, uid} ->
    @tag compliance: "limit rows on #{column} from #{table} in subquery"
    test "limit rows on #{column} from #{table} in subquery", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT MEDIAN(x) FROM (
          SELECT #{unquote(uid)}, #{unquote(column)} AS x FROM #{unquote(table)} ORDER BY 1, 2 LIMIT 50
        ) t
      """)
    end
  end)
end
