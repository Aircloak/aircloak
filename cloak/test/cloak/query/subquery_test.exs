defmodule Cloak.Query.SubqueryTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights_sq", "height INTEGER, name TEXT")
    Cloak.Test.DB.clear_table("heights_sq")
    :ok = insert_rows(_user_ids = 1..100, "heights_sq", ["height"], [180])
  end

  test "selecting from a subquery" do
    assert_query "select height from (select user_id, height from heights_sq) alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "column alias in a subquery" do
    assert_query "select h from (select user_id, height as h from heights_sq) alias",
      %{columns: ["h"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "user_id can be in any position in a subquery" do
    assert_query "select height from (select height, user_id from heights_sq) alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "fully qualified names with subqueries" do
    assert_query "select alias.height from (select user_id, height from heights_sq) alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
  end

  test "joining two subqueries" do
    assert_query(
      """
        select t1.height as h1, t2.height as h2 from
          (select user_id, height from heights_sq) t1
          inner join (select user_id, height from heights_sq) t2 on t1.user_id = t2.user_id
      """,
      %{columns: ["h1", "h2"], rows: [%{row: [180, 180], occurrences: 100}]}
    )
  end

  test "joining a subquery and a table" do
    assert_query(
      """
        select t1.height as h1, heights_sq.height as h2 from
          (select user_id, height from heights_sq) t1
          inner join heights_sq on heights_sq.user_id = t1.user_id
      """,
      %{columns: ["h1", "h2"], rows: [%{row: [180, 180], occurrences: 100}]}
    )
  end

  test "nesting subqueries" do
    assert_query(
      """
        select height from (
          select user_id, height from (select user_id, height from heights_sq) inner_alias
        ) outer_alias
      """,
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end
end
