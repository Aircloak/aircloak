defmodule Compliance.Isolators.Test do
  use ComplianceCase, async: true

  [
    {"height", "users"},
    {"age", "users"},
    {"id", "notes"},
    {"user_fk", "addresses"},
    {"home.postal_code", "addresses"},
    {"work.postal_code", "addresses"},
    {"user_fk", "notes"},
    {"note_id", "notes_changes"},
    {"date", "notes_changes"},
    {"birthday", "users"},
    {"name", "users"},
    {"column_with_a_very_long_name", "users"},
    {"home.city", "addresses"},
    {"work.city", "addresses"},
    {"title", "notes"},
    {"content", "notes"},
    {"change", "notes_changes"},
    {"nullable", "users"}
  ]
  |> Enum.each(fn {column, table} ->
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
