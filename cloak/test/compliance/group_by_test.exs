defmodule Compliance.GroupByTest do
  use ComplianceCase, async: true

  Enum.each(all_columns(), fn {column, table} ->
    @tag compliance: "#{column} #{table} group by"
    test "group by #{column} in query on #{table}", context do
      context
      |> disable_unicode(unquote(table), unquote(column))
      |> disable_for(Cloak.DataSource.ClouderaImpala, unquote(column) == "birthday")
      |> disable_for(Cloak.DataSource.Oracle, unquote(column) == "birthday")
      |> disable_for(:all, unquote(table) == "users_public" and unquote(column) == "name_unicode")
      |> assert_consistent_and_not_failing("""
        SELECT #{unquote(column)}, COUNT(DISTINCT user_id)
        FROM #{unquote(table)} GROUP BY 1 ORDER BY 1 ASC NULLS FIRST
      """)
    end
  end)

  @tag compliance: "empty group by"
  test "empty group by", context do
    assert_consistent_and_not_failing(context, "SELECT 0 FROM users_public GROUP BY ()")
  end

  Enum.each(all_columns(), fn {column, table} ->
    @tag compliance: "#{column} #{table} subquery grouping sets"
    test "grouping sets for #{column} in subquery on #{table}", context do
      context
      |> disable_unicode(unquote(table), unquote(column))
      |> assert_consistent_and_not_failing("""
        SELECT COUNT(*), COUNT(DISTINCT uid), STDDEV(c) FROM (
          SELECT user_id AS uid, #{unquote(column)}, COUNT(*) AS c
          FROM #{unquote(table)} GROUP BY GROUPING SETS (1, (1, 2)) ORDER BY 1, 2
        ) t
      """)
    end
  end)

  @tag compliance: "grouping_id in standard query"
  test "grouping_id in standard query", context do
    context
    |> assert_consistent_and_not_failing("""
      SELECT age, height, cast(birthday as date), GROUPING_ID(age, height, cast(birthday as date)) FROM users_public
      GROUP BY GROUPING SETS ((), 1, 2, 3, (1, 2), (2, 3), (1, 2, 3))
      ORDER BY 1 NULLS FIRST, 2 NULLS FIRST, 3 NULLS FIRST, 4
    """)
  end

  Enum.each(all_columns(), fn {column, table} ->
    @tag compliance: "#{column} #{table} uid-query grouping sets"
    test "grouping sets for #{column} in uid-based query on #{table}", context do
      context
      |> disable_unicode(unquote(table), unquote(column))
      |> disable_for(Cloak.DataSource.ClouderaImpala, unquote(column) == "birthday")
      |> disable_for(Cloak.DataSource.Oracle, unquote(column) == "birthday")
      |> disable_for(
        Cloak.DataSource.Oracle,
        unquote(table) == "users_public" and unquote(column) == "column_with_a_very_long_name"
      )
      |> assert_consistent_and_not_failing("""
        SELECT #{unquote(column)}, COUNT(*), COUNT(DISTINCT user_id), STDDEV(0) FROM
        #{unquote(table)} GROUP BY GROUPING SETS ((), 1) ORDER BY 1 ASC NULLS FIRST, 2, 3
      """)
    end
  end)

  Enum.each(all_columns(), fn {column, table} ->
    @tag compliance: "#{column} #{table} stats-query grouping sets"
    test "grouping sets for #{column} in stats-based query on #{table}", context do
      context
      |> disable_unicode(unquote(table), unquote(column))
      # Impala maps dates to datetime (TIMESTAMP). The returned results will be equivalent,
      # but tests will fail because other databases return results as date only.
      |> disable_for(Cloak.DataSource.ClouderaImpala, unquote(column) == "birthday")
      |> disable_for(Cloak.DataSource.Oracle, unquote(column) == "birthday")
      |> disable_for(
        Cloak.DataSource.Oracle,
        unquote(table) == "users_public" and unquote(column) == "column_with_a_very_long_name"
      )
      |> assert_consistent_and_not_failing("""
        SELECT #{unquote(column)}, COUNT(*), COUNT(DISTINCT user_id) FROM
        #{unquote(table)} GROUP BY GROUPING SETS ((), 1) ORDER BY 1 ASC NULLS FIRST, 2, 3
      """)
    end
  end)

  defp disable_unicode(context, table, column) do
    context
    |> disable_for(Cloak.DataSource.ClouderaImpala, column == "name_unicode")
    |> disable_for(:all, table == "users_public" and column == "name_unicode")
  end
end
