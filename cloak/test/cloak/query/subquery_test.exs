defmodule Cloak.Query.SubqueryTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    Cloak.Test.DB.setup()
    Cloak.Test.DB.create_test_schema()
    :ok = Cloak.Test.DB.create_table("heights", "height INTEGER, name TEXT")
  end

  setup do
    Cloak.Test.DB.clear_table("heights")
    :ok
  end

  test "selecting from a subquery" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from (select user_id, height from heights) alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "column alias in a subquery" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select h from (select user_id, height as h from heights) alias",
      %{columns: ["h"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "user_id can be in any position in a subquery" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select height from (select height, user_id from heights) alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "fully qualified names with subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query "select alias.height from (select user_id, height from heights) alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "joining two subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query(
      """
        select t1.height as h1, t2.height as h2 from
          (select user_id, height from heights) t1
          inner join (select user_id, height from heights) t2 on t1.user_id = t2.user_id
      """,
      %{columns: ["h1", "h2"], rows: [%{row: [180, 180], occurrences: 100}]}
    )
  end

  test "joining a subquery and a table" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query(
      """
        select t1.height as h1, heights.height as h2 from
          (select user_id, height from heights) t1
          inner join heights on heights.user_id = t1.user_id
      """,
      %{columns: ["h1", "h2"], rows: [%{row: [180, 180], occurrences: 100}]}
    )
  end

  test "nesting subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query(
      """
        select height from (
          select user_id, height from (select user_id, height from heights) inner_alias
        ) outer_alias
      """,
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "arithmetic expressions in subqueries" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query(
      """
        select height from (
          select user_id, (height - 1 + 4 / 2 * 3 + 3 ^ 2) as height from heights
        ) alias
      """,
      %{columns: ["height"], rows: [%{row: [194.0], occurrences: 100}]}
    )
  end

  test "trunc/1" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query(
      """
        select height from (
          select user_id, trunc(height + 0.6) as height from heights
        ) alias
      """,
      %{columns: ["height"], rows: [%{row: [180.0], occurrences: 100}]}
    )
  end

  test "trunc/2" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    assert_query(
      """
        select height from (
          select user_id, trunc(height + 0.126, 2) as height from heights
        ) alias
      """,
      %{columns: ["height"], rows: [%{row: [180.12], occurrences: 100}]}
    )
  end
end
