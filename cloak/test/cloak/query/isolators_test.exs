defmodule Cloak.Query.Isolators.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok =
      Cloak.Test.DB.create_table(
        "query_isolators",
        "isolating INTEGER, regular INTEGER, isolating_string TEXT, regular_string TEXT, isolating_date DATE," <>
          " regular_date DATE"
      )

    for data_source <- Cloak.DataSource.all(),
        column <- ~w(isolating isolating_string isolating_date) do
      Cloak.TestIsolatorsCache.register_isolator(data_source, "query_isolators", column)
    end

    :ok
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

  test "string functions are allowed on isolators" do
    assert_allowed("SELECT COUNT(*) FROM query_isolators WHERE trim($col_string) = 'something'")
  end

  test "implicit range functions are allowed on isolators (see anonymization.md#isolating-columns)" do
    assert_allowed("SELECT COUNT(*) FROM query_isolators WHERE month($col_date) = 1")
    assert_allowed("SELECT COUNT(*) FROM query_isolators WHERE bucket($col by 10) = 10")
  end

  test "aliased tables" do
    assert_forbidden("SELECT COUNT(*) FROM query_isolators AS qi WHERE $col IN (1, 2)")
  end

  test "subqueries" do
    assert_forbidden("SELECT COUNT(*) FROM (SELECT user_id, $col AS x FROM query_isolators) y WHERE x IN (1, 2)")
  end

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
