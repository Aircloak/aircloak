defmodule Cloak.Query.Shadows.Test do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("query_shadows", "value INTEGER")
  end

  setup do
    :ok = Cloak.Test.DB.clear_table("query_shadows")
  end

  test "allows up to 2 <> conditions with rare values" do
    :ok = insert_rows(_user_ids = 1..20, "query_shadows", ["value"], [10])
    :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["value"], [1])
    :ok = insert_rows(_user_ids = 1..1, "query_shadows", ["value"], [2])

    assert_query("SELECT COUNT(*) FROM query_shadows WHERE value <> 1 AND value <> 2", %{rows: [%{row: [20]}]})
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
end
