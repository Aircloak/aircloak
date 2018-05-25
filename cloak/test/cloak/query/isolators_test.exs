defmodule Cloak.Query.Isolators.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("query_isolators", "isolating INTEGER, regular INTEGER")

    :ok =
      Cloak.Test.DB.add_users_data("query_isolators", ["isolating", "regular"], [
        ["user1", 1, 1],
        ["user2", 2, 1],
        ["user3", 3, 1],
        ["user4", 4, 1]
      ])
  end

  test "clear conditions are allowed for isolators" do
    assert_allowed("SELECT COUNT(*) FROM query_isolators WHERE $col = 10")
  end

  test "conditions with math are forbidden for isolators" do
    assert_forbidden("SELECT COUNT(*) FROM query_isolators WHERE sqrt($col) = 10")
  end

  test "IN is forbidden" do
    assert_forbidden("SELECT COUNT(*) FROM query_isolators WHERE $col IN (1, 2)")
  end

  test "IN with a single item is allowed as synonymous to =" do
    assert_allowed("SELECT COUNT(*) FROM query_isolators WHERE $col IN (1)")
  end

  test "ranges are allowed on isolators" do
    assert_allowed("SELECT COUNT(*) FROM query_isolators WHERE $col BETWEEN 0 AND 10")
  end

  test "IS NULL is allowed on isolators" do
    assert_allowed("SELECT COUNT(*) FROM query_isolators WHERE $col IS NULL")
  end

  test "date extractors"

  test "math in GROUP BY"

  test "string functions"

  defp assert_allowed(query) do
    for column <- ["isolating", "regular"] do
      query |> String.replace("$col", column) |> assert_query(%{rows: [_ | _]})
    end
  end

  defp assert_forbidden(query) do
    query |> String.replace("$col", "regular") |> assert_query(%{rows: [_ | _]})
    query |> String.replace("$col", "isolating") |> assert_query(%{error: error})
    assert error =~ ~r/The column `isolating.*` is isolating/
  end
end
