defmodule Compliance.GroupByTest do
  use ComplianceCase, async: true

  Enum.each(all_columns(), fn {column, table, uid} ->
    @tag compliance: "#{column} #{table} group by"
    test "group by #{column} in query on #{table}", context do
      context
      |> disable_for(Cloak.DataSource.SQLServer, unquote(column) == "name")
      |> disable_for(Cloak.DataSource.MySQL, unquote(column) == "name")
      |> disable_for(Cloak.DataSource.MongoDB, unquote(column) == "birthday")
      |> disable_for(Cloak.DataSource.Drill, unquote(column) != "name")
      |> assert_consistent_and_not_failing("""
        SELECT #{unquote(column)}, COUNT(DISTINCT #{unquote(uid)})
        FROM #{unquote(table)} GROUP BY 1 ORDER BY 1
      """)
    end
  end)
end
