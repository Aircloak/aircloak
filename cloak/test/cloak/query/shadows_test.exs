defmodule Cloak.Query.Shadows.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("query_shadows", "value INTEGER, string TEXT")

    for data_source <- Cloak.DataSource.all() do
      Cloak.TestShadowCache.live(data_source, "query_shadows", "value")
      Cloak.TestShadowCache.live(data_source, "query_shadows", "string")
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

      assert_query("SELECT COUNT(*) FROM query_shadows WHERE value <> 1 AND value <> 2 AND value <> 10", %{
        rows: [%{row: [0]}]
      })
    end

    test "allows any number of <> conditions with popular values" do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [10])
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [20])
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [30])

      assert_query(
        "SELECT COUNT(*) FROM query_shadows WHERE value <> 10 AND value <> 20 AND value <> 30",
        %{rows: [%{row: [0]}]}
      )
    end

    test "forbids more than 2 <> conditions with rare values" do
      assert_query("SELECT COUNT(*) FROM query_shadows WHERE value NOT IN (1, 2, 3)", %{error: error})

      assert error =~ ~r/At most 2 negative conditions/
    end
  end

  describe "NOT LIKE" do
    setup do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["string"], ["aaa"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["bbb"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["ccc"])
    end

    test "allows up to 2 conditions with rare values" do
      assert_query(
        """
          SELECT COUNT(*) FROM query_shadows
          WHERE string NOT LIKE '%a%' AND string NOT LIKE '_b_' AND string NOT LIKE 'cc_'
        """,
        %{rows: [%{row: [0]}]}
      )
    end

    test "allows any number of conditions with popular values" do
      assert_query(
        """
          SELECT COUNT(*) FROM query_shadows
          WHERE string NOT LIKE '%a%' AND string NOT LIKE '_a_' AND string NOT LIKE 'aa_'
        """,
        %{rows: [%{row: [0]}]}
      )
    end

    test "forbids more than 2 conditions with rare values" do
      assert_query(
        """
          SELECT COUNT(*) FROM query_shadows
          WHERE string NOT LIKE '%b%' AND string NOT LIKE '_A_' AND string NOT LIKE 'cc_'
        """,
        %{error: error}
      )

      assert error =~ ~r/At most 2 negative conditions/
    end
  end

  describe "NOT ILIKE" do
    test "applied case-isensitively" do
      :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["string"], ["aaa"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["bbb"])
      :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["string"], ["ccc"])

      assert_query(
        """
          SELECT COUNT(*) FROM query_shadows
          WHERE string NOT ILIKE 'aAa' AND string NOT ILIKE 'AaA' AND string NOT ILIKE 'AAA'
        """,
        %{rows: [%{row: [0]}]}
      )
    end
  end

  test "condition on column from subquery" do
    assert_query(
      """
        SELECT COUNT(*) FROM (
          SELECT user_id, value FROM query_shadows
        ) foo
        WHERE value NOT IN (1, 2, 3)
      """,
      %{error: error}
    )

    assert error =~ ~r/At most 2 negative conditions/
  end

  test "condition on expression" do
    assert_query("SELECT COUNT(*) FROM query_shadows WHERE lower(string) NOT IN ('1', '2', '3')", %{error: error})

    assert error =~ ~r/At most 2 negative conditions/
  end

  test "conditions with constants are always safe" do
    assert_query("SELECT COUNT(*) FROM query_shadows WHERE '1' NOT IN ('1', '2', '3')", %{rows: [%{row: [0]}]})
  end
end
