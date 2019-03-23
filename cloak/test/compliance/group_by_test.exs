defmodule Compliance.GroupByTest do
  use ComplianceCase, async: true

  Enum.each(all_columns(), fn {column, table, uid} ->
    @tag compliance: "#{column} #{table} group by"
    test "group by #{column} in query on #{table}", context do
      context
      |> disable_unicode(unquote(table), unquote(column))
      |> disable_for(Cloak.DataSource.MongoDB, unquote(column) == "birthday")
      |> assert_consistent_and_not_failing("""
        SELECT #{unquote(column)}, COUNT(DISTINCT #{unquote(uid)})
        FROM #{unquote(table)} GROUP BY 1 ORDER BY 1 ASC NULLS FIRST
      """)
    end
  end)

  @tag compliance: "empty group by"
  test "empty group by", context do
    assert_consistent_and_not_failing(context, "SELECT NULL FROM users_public GROUP BY ()")
  end

  Enum.each(all_columns(), fn {column, table, uid} ->
    @tag compliance: "#{column} #{table} subquery grouping sets"
    test "grouping sets for #{column} in subquery on #{table}", context do
      context
      |> disable_unicode(unquote(table), unquote(column))
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*), COUNT(uid), COUNT(column) FROM (
          SELECT #{unquote(uid)} AS uid, #{unquote(column)} AS column
          FROM #{unquote(table)} GROUP BY GROUPING SETS ((), 1, (1, 2))
        ) t
      """)
    end
  end)

  Enum.each(all_columns(), fn {column, table, uid} ->
    @tag compliance: "#{column} #{table} query grouping sets"
    test "grouping sets for #{column} in query on #{table}", context do
      context
      |> disable_unicode(unquote(table), unquote(column))
      |> disable_for(Cloak.DataSource.MongoDB, unquote(column) == "birthday")
      |> assert_consistent_and_not_failing("""
        SELECT #{unquote(column)}, COUNT(*), COUNT(#{unquote(uid)}) FROM 
        #{unquote(table)} GROUP BY GROUPING SETS ((), 1)
      """)
    end
  end)

  defp disable_unicode(context, table, column) do
    context
    |> disable_for(Cloak.DataSource.SQLServer, column == "name")
    |> disable_for(Cloak.DataSource.MySQL, column == "name")
    |> disable_for(:all, table == "users_public" and column == "name")
  end
end
