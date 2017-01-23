defmodule Cloak.Query.AnonymizationTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("anonymizations", "number REAL")
    :ok = Cloak.Test.DB.create_table("other_table", "dummy BOOLEAN")
  end

  setup do
    Cloak.Test.DB.clear_table("anonymizations")
    Cloak.Test.DB.clear_table("other_table")
    :ok
  end

  test "shrink and drop" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [6])
    :ok = insert_rows(_user_ids = 0..0, "anonymizations", ["number"], [2])
    assert_query "select count(*) from anonymizations where number >= 0 and number < 10",
      %{columns: ["count"], rows: [%{row: [100]}]}
  end

  test "recursive shrink and drop" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [6])
    :ok = insert_rows(_user_ids = 0..0, "anonymizations", ["number"], [8])
    assert_query "select count(*) from anonymizations where number >= 0 and number < 20",
      %{columns: ["count"], rows: [%{row: [100]}]}
  end

  test "having range in subquery" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [180])
    :ok = insert_rows(_user_ids = [1], "anonymizations", ["number"], [0])

    assert_query("""
      SELECT count(*)
      FROM (
        SELECT user_id
        FROM anonymizations
        GROUP BY user_id
        HAVING avg(number) > 0 AND avg(number) < 200
      ) x
    """, %{columns: ["count"], rows: [%{row: [99]}]})
  end

  test "having range in nested subquery" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [180])
    :ok = insert_rows(_user_ids = [1], "anonymizations", ["number"], [0])

    assert_query("""
      SELECT count(*)
      FROM (
        SELECT user_id
        FROM (
          SELECT user_id
          FROM anonymizations
          GROUP BY user_id
          HAVING avg(number) > 0 AND avg(number) < 200
        ) y
        GROUP BY user_id
      ) x
    """, %{columns: ["count"], rows: [%{row: [99]}]})
  end

  test "where range in subquery" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [180])
    :ok = insert_rows(_user_ids = [101], "anonymizations", ["number"], [185])

    assert_query("""
      SELECT avg(foo)
      FROM (
        SELECT user_id, sum(number) as foo
        FROM anonymizations
        WHERE number > 0 and number < 200
        GROUP BY user_id
      ) x
    """, %{columns: ["avg"], rows: [%{row: [180.0]}]})
  end

  test "where range in nested subquery" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [180])
    :ok = insert_rows(_user_ids = [101], "anonymizations", ["number"], [185])

    assert_query("""
      SELECT count(*)
      FROM (
        SELECT user_id
        FROM (
          SELECT user_id, sum(number)
          FROM anonymizations
          WHERE number > 0 and number < 200
          GROUP BY user_id
        ) y
        GROUP BY user_id
      ) x
    """, %{columns: ["count"], rows: [%{row: [100]}]})
  end

  test "range in join condition" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [180])
    :ok = insert_rows(_user_ids = [101], "anonymizations", ["number"], [185])
    :ok = insert_rows(_user_ids = 1..101, "other_table", [], [])

    assert_query("""
      SELECT count(*)
      FROM (
        SELECT anonymizations.user_id
        FROM anonymizations
        JOIN other_table
          ON anonymizations.user_id = other_table.user_id
          AND anonymizations.number > 0
          AND anonymizations.number < 200
      ) x
    """, %{columns: ["count"], rows: [%{row: [100]}]})
  end

  test "range in nested join condition" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [180])
    :ok = insert_rows(_user_ids = [101], "anonymizations", ["number"], [185])
    :ok = insert_rows(_user_ids = 1..101, "other_table", [], [])

    assert_query("""
      SELECT count(*) FROM (
        SELECT user_id FROM (
          SELECT anonymizations.user_id AS user_id
          FROM anonymizations
          JOIN other_table
            ON anonymizations.user_id = other_table.user_id
            AND anonymizations.number > 0
            AND anonymizations.number < 200
        ) x
      ) y
    """, %{columns: ["count"], rows: [%{row: [100]}]})
  end
end
