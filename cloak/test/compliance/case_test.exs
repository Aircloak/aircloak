defmodule Compliance.CaseTest do
  use ComplianceCase, async: true

  Enum.each(integer_columns(), fn {column, table} ->
    @tag compliance: "#{column} #{table} uid bucketed case"
    test "uid-based bucketed case on #{column} in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT CASE WHEN #{unquote(column)} = 1 THEN TRUE ELSE FALSE END, COUNT(*), STDDEV(0)
        FROM #{unquote(table)}
        GROUP BY 1 ORDER BY 1, 2
      """)
    end

    @tag compliance: "#{column} #{table} stats bucketed case"
    test "stats-based bucketed case on #{column} in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT CASE WHEN #{unquote(column)} = 1 THEN TRUE ELSE FALSE END, COUNT(*)
        FROM #{unquote(table)}
        GROUP BY 1 ORDER BY 1, 2
      """)
    end

    @tag compliance: "#{column} #{table} uid aggregated case"
    test "uid-based aggregated case on #{column} in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT AVG(CASE WHEN #{unquote(column)} = 1 THEN NULL WHEN #{unquote(column)} = 2 THEN NULL ELSE 1 END), STDDEV(0)
        FROM #{unquote(table)}
        ORDER BY 1
      """)
    end

    @tag compliance: "#{column} #{table} stats aggregated case"
    test "stats-based aggregated case on #{column} in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT AVG(CASE WHEN #{unquote(column)} = 1 THEN NULL WHEN #{unquote(column)} = 2 THEN NULL ELSE 1 END)
        FROM #{unquote(table)}
        ORDER BY 1
      """)
    end
  end)
end
