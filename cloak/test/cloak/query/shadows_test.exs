defmodule Cloak.Query.Shadows.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("query_shadows", "value INTEGER, string TEXT")
    :ok = Cloak.Test.DB.create_table("query_shadows_userless", "value INTEGER", user_id: nil)

    for data_source <- Cloak.DataSource.all() do
      Cloak.TestShadowCache.live(data_source, "query_shadows", "value")
      Cloak.TestShadowCache.live(data_source, "query_shadows", "string")
      Cloak.TestShadowCache.live(data_source, "query_shadows_userless", "value")
    end

    :ok
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("query_shadows")
  end

  describe "<>" do
    test "allows up to 2 <> conditions with rare values" do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [10])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["value"], [1])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["value"], [2])

      assert_allowed("SELECT COUNT(*) FROM query_shadows WHERE value <> 1 AND value <> 2 AND value <> 10")
    end

    test "allows any number of <> conditions with popular values" do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [10])
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [20])
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [30])

      assert_allowed("SELECT COUNT(*) FROM query_shadows WHERE value <> 10 AND value <> 20 AND value <> 30")
    end

    test "forbids more than 2 <> conditions with rare values" do
      assert_forbidden("SELECT COUNT(*) FROM query_shadows WHERE value NOT IN (1, 2, 3)")
    end
  end

  describe "NOT LIKE" do
    setup do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["string"], ["aaa"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["bbb"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["ccc"])
    end

    test "allows up to 2 conditions with rare values" do
      assert_allowed("""
        SELECT COUNT(*) FROM query_shadows
        WHERE string NOT LIKE '%a%' AND string NOT LIKE '_b_' AND string NOT LIKE 'cc_'
      """)
    end

    test "allows any number of conditions with popular values" do
      assert_allowed("""
        SELECT COUNT(*) FROM query_shadows
        WHERE string NOT LIKE '%a%' AND string NOT LIKE '_a_' AND string NOT LIKE 'aa_'
      """)
    end

    test "forbids more than 2 conditions with rare values" do
      assert_forbidden("""
        SELECT COUNT(*) FROM query_shadows
        WHERE string NOT LIKE '%b%' AND string NOT LIKE '_A_' AND string NOT LIKE 'cc_'
      """)
    end
  end

  describe "NOT ILIKE" do
    test "applied case-isensitively" do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["string"], ["aaa"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["bbb"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["ccc"])

      assert_allowed("""
        SELECT COUNT(*) FROM query_shadows
        WHERE string NOT ILIKE 'aAa' AND string NOT ILIKE 'AaA' AND string NOT ILIKE 'AAA'
      """)
    end
  end

  describe "subqueries" do
    test "condition on column from subquery" do
      assert_forbidden("""
        SELECT COUNT(*) FROM (
          SELECT user_id, value FROM query_shadows
        ) foo
        WHERE value NOT IN (1, 2, 3)
      """)
    end

    test "condition are counted across subqueries" do
      assert_forbidden("""
        SELECT COUNT(*) FROM (
          SELECT user_id, value FROM query_shadows
          WHERE value <> 3
        ) foo
        WHERE value NOT IN (1, 2)
      """)
    end

    test "condition on popular value from subquery" do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [1])

      assert_allowed("""
        SELECT COUNT(*) FROM (
          SELECT user_id, value AS foo FROM query_shadows
          WHERE value <> 3
        ) foo
        WHERE foo NOT IN (1, 2)
      """)
    end
  end

  describe "userless table" do
    test "no anonymizing queries" do
      assert_allowed("SELECT COUNT(*) FROM query_shadows_userless WHERE value NOT IN (1, 2)")
    end
  end

  test "condition on expression" do
    assert_forbidden("SELECT COUNT(*) FROM query_shadows WHERE lower(string) NOT IN ('1', '2', '3')")
  end

  test "conditions with constants are always safe" do
    assert_allowed("SELECT COUNT(*) FROM query_shadows WHERE '1' NOT IN ('1', '2', '3')")
  end

  defp assert_allowed(query), do: assert_query(query, %{rows: [%{row: _} | _]})

  defp assert_forbidden(query) do
    assert_query(query, %{error: error})
    assert error =~ ~r/At most 2 negative conditions/
  end
end
