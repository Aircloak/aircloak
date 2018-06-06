defmodule Compliance.Isolators.Test do
  use ComplianceCase, async: true

  Enum.each(all_columns(), fn {column, table, _} ->
    @tag compliance: "isolator query on #{column} in #{table}"
    test "isolator query on #{column} from #{table}", context do
      results =
        for data_source <- context.data_sources do
          Cloak.DataSource.Isolators.Query.isolates_users?(data_source, unquote(table), unquote(column))
        end

      assert results |> Enum.uniq() |> Enum.count() == 1
    end
  end)
end
