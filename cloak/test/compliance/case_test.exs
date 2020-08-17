defmodule Compliance.CaseTest do
  use ComplianceCase, async: true

  setup_all do
    Enum.each(integer_columns(), fn {column, table} ->
      # handle joined tables and fully specified columns
      table = String.split(table) |> Enum.at(0)
      column = String.split(column, ".") |> Enum.at(-1)
      for data_source <- Cloak.DataSource.all(), do: Cloak.TestShadowCache.safe(data_source, table, column, [1, 2])
    end)
  end

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
        SELECT AVG(CASE WHEN #{unquote(column)} = 1 THEN 0 WHEN #{unquote(column)} = 2 THEN 0 ELSE 1 END), STDDEV(0)
        FROM #{unquote(table)}
        ORDER BY 1
      """)
    end

    @tag compliance: "#{column} #{table} stats aggregated case"
    test "stats-based aggregated case on #{column} in query on #{table}", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT AVG(CASE WHEN #{unquote(column)} = 1 THEN 0 WHEN #{unquote(column)} = 2 THEN 0 ELSE 1 END)
        FROM #{unquote(table)}
        ORDER BY 1
      """)
    end
  end)
end
