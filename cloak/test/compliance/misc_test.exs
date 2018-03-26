defmodule Compliance.MiscTest do
  use ComplianceCase, async: true

  Enum.each(table_uids(), fn {table, uid} ->
    @tag compliance: "sample users from #{table} in query"
    test "sample users from #{table} test in query", context do
      assert_consistent_and_not_failing(context, """
        SELECT COUNT(*), COUNT(DISTINCT #{unquote(uid)}) FROM #{unquote(table)} SAMPLE_USERS 25%
      """)
    end

    @tag compliance: "sample users from #{table} in subquery"
    test "sample users from #{table} test in subquery", context do
      assert_consistent_and_not_failing(context, """
        SELECT COUNT(*) FROM (SELECT DISTINCT #{unquote(uid)} FROM #{unquote(table)} SAMPLE_USERS 25%) AS t
      """)
    end
  end)
end
