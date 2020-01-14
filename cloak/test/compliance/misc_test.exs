defmodule Compliance.MiscTest do
  use ComplianceCase, async: true

  @moduletag :works

  Enum.each(numerical_columns(), fn {column, table} ->
    @tag compliance: "limit rows on #{column} from #{table} in subquery"
    test "limit rows on #{column} from #{table} in subquery", context do
      context
      |> assert_consistent_and_not_failing("""
        SELECT STDDEV(CAST(x AS real)) FROM (
          SELECT user_id, #{unquote(column)} AS x FROM #{unquote(table)} ORDER BY 1, 2 LIMIT 50
        ) t
      """)
    end
  end)
end
