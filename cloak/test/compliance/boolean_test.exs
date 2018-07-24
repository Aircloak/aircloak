defmodule Compliance.BooleanTest do
  use ComplianceCase, async: true

  Enum.each(numerical_columns() |> raw_columns(), fn {column, table, uid} ->
    @tag compliance: "#{column} #{table} select boolean in query"
    test "input cast(#{column} as boolean) in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT CAST(#{unquote(column)} AS boolean)
        FROM #{unquote(table)}
        ORDER BY 1
      """)
    end

    @tag compliance: "#{column} #{table} select boolean in sub-query"
    test "input cast(#{column} as boolean) in sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT output FROM (
          SELECT
            #{unquote(uid)},
            CAST(#{unquote(column)} AS boolean) AS output
          FROM #{unquote(table)}
        ) table_alias
        ORDER BY 1
      """)
    end

    @tag compliance: "#{column} #{table} filter boolean in query"
    test "filter cast(#{column} as boolean) in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*)
        FROM #{unquote(table)}
        WHERE CAST(#{unquote(column)} AS boolean)
      """)
    end

    @tag compliance: "#{column} #{table} select boolean in sub-query"
    test "filter cast(#{column} as boolean) in sub-query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*) FROM (
          SELECT
            #{unquote(uid)}
          FROM #{unquote(table)}
          WHERE CAST(#{unquote(column)} AS boolean)
        ) table_alias
      """)
    end
  end)
end
