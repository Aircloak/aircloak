defmodule Compliance.AggregationTest do
  use ComplianceCase, async: true

  Enum.each(all_columns(), fn {column, table, uid} ->
    @tag compliance: "#{column} #{table} count"
    test "count(#{column}) in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(#{unquote(column)}) FROM #{unquote(table)}
      """)
    end

    @tag compliance: "#{column} #{table} count distinct"
    test "count(distinct #{column}) in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(DISTINCT #{unquote(column)}) FROM #{unquote(table)}
      """)
    end

    @tag compliance: "#{column} #{table} group by"
    test "group by #{column} in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT #{unquote(column)}, COUNT(DISTINCT #{unquote({uid})})
        FROM #{unquote(table)} GROUP BY 1 ORDER BY 1
      """)
    end
  end)
end
