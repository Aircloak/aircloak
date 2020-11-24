defmodule Cloak.Query.SubqueryTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights_sq", "height INTEGER, name TEXT")
    Cloak.TestShadowCache.safe(default_data_source(), "heights_sq", "height", [170, 180])
  end

  setup do
    Cloak.Test.DB.clear_table("heights_sq")
    :ok = insert_rows(_user_ids = 1..100, "heights_sq", ["height", "name"], [180, "Tom"])
  end

  test "selecting from a subquery" do
    assert_query("select height from (select user_id, height from heights_sq) alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "selecting all from a subquery" do
    assert_query("select height from (select alias.* from (select user_id, height from heights_sq) alias) t", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "implicitly selecting user id in a subquery" do
    assert_query("select height from (select height from heights_sq) alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "implicitly selecting user id in a subquery with group by" do
    assert_query("select height from (select max(height) as height from heights_sq group by user_id) alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "implicitly selecting user id in a nested subquery" do
    assert_query("select height from (select height from (select height from heights_sq) sq1) sq2", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "selecting all from a subquery with an implicitly selected user id" do
    assert_query("select height from (select * from (select height from heights_sq) sq1) sq2", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "column alias in a subquery" do
    assert_query("select h from (select user_id, height as h from heights_sq) alias", %{
      columns: ["h"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "where alias in a subquery" do
    assert_query("select h from (select user_id, height as h from heights_sq where h = 180) alias", %{
      columns: ["h"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "user_id can be in any position in a subquery" do
    assert_query("select height from (select height, user_id from heights_sq) alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "fully qualified names with subqueries" do
    assert_query("select alias.height from (select user_id, height from heights_sq) alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "fully qualified names with a quoted alias" do
    assert_query(~s/select "ali as".height from (select user_id, height from heights_sq) "ali as"/, %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
  end

  test "where in a subquery" do
    assert_query(
      "select height from (select user_id, height from heights_sq where height >= 0 and height < 200) alias",
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "having in a subquery" do
    assert_query(
      "select count(*) from (select user_id from heights_sq group by 1 having max(height) in (170, 180)) alias",
      %{rows: [%{row: [100]}]}
    )
  end

  test "function in where in a subquery" do
    assert_query("select height from (select user_id, height from heights_sq where height / 2 = 90) alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 100}]
    })
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

  test "limit and offset in subqueries" do
    :ok = insert_rows(_user_ids = 101..150, "heights_sq", ["height"], [190])

    assert_query("select height from (select user_id, height from heights_sq order by height limit 50) alias", %{
      columns: ["height"],
      rows: [%{row: [180], occurrences: 50}]
    })

    assert_query("select height from (select user_id, height from heights_sq order by height offset 50) alias", %{
      error: "`OFFSET` clause requires a `LIMIT` clause in `restricted` subqueries." <> _
    })

    assert_query(
      "select height from (select user_id, height from heights_sq order by height limit 50 offset 100)" <> " alias",
      %{columns: ["height"], rows: [%{row: [190], occurrences: 50}]}
    )
  end

  test "non selected order by in subquery" do
    assert_query("select x from (select user_id, 3.14 as x from heights_sq order by height) alias", %{
      columns: ["x"],
      rows: [%{row: [3.14], occurrences: 100}]
    })
  end

  test "non selected aggregate in subquery" do
    :ok = insert_rows(_user_ids = 1..10, "heights_sq", ["height"], [160])
    :ok = insert_rows(_user_ids = 11..30, "heights_sq", ["height"], [170])
    :ok = insert_rows(_user_ids = 31..60, "heights_sq", ["height"], [180])

    assert_query(
      "select height from (select user_id, height from heights_sq group by 1, 2 order by count(*) desc limit 1) alias",
      %{columns: ["height"], rows: [%{row: [180]}]}
    )
  end

  test "group by with having in subqueries" do
    :ok = insert_rows(_user_ids = 1..50, "heights_sq", ["height"], [170])

    assert_query(
      """
      select count(h) from
      (select user_id, avg(height) as h from heights_sq
      group by user_id
      having max(height) = min(height)) alias
      """,
      %{columns: ["count"], rows: [%{row: [50], occurrences: 1}]}
    )
  end

  test "subquery can handle multiple functions" do
    assert_query(
      """
        select min_h, max_h
        from (select user_id, min(height) as min_h, max(height) as max_h
              from heights_sq
              group by user_id
             ) t
      """,
      %{columns: ["min_h", "max_h"], rows: [%{occurrences: 100, row: [180, 180]}]}
    )
  end

  test "using view in a subquery" do
    assert_query(
      "select height from (select user_id, height from heights_view) alias",
      [views: %{"heights_view" => %{sql: "select user_id, height from heights_sq"}}],
      %{columns: ["height"], rows: [%{row: [180], occurrences: 100}]}
    )
  end

  test "binding parameters in a subquery" do
    assert_query(
      "select height + $1 as height from (select user_id, height + $2 as height from heights_sq) alias",
      [parameters: [%{type: :integer, value: 10}, %{type: :integer, value: 20}]],
      %{columns: ["height"], rows: [%{row: [210], occurrences: 100}]}
    )
  end

  test "[Issue #3191] grouping by user_id-derived column causes invalid noise layers" do
    assert_query(
      """
        select count(*) from (
          select user_id, upper(user_id) as uid2 from (
            select user_id from heights_sq
          ) t
          group by 1, 2
        ) t
      """,
      %{rows: [%{row: [100], occurrences: 1}]}
    )
  end

  test "[BUG] group by multiple columns" do
    assert_query(
      "select count(*) from (select user_id, height, name from heights_sq group by 1, 2, 3) t",
      %{columns: ["count"], rows: [%{row: [100], occurrences: 1}]}
    )
  end

  test "grouping sets" do
    assert_query(
      "select count(*) from (select user_id, height, name from heights_sq group by grouping sets ((1, 2), (1, 3))) t",
      %{columns: ["count"], rows: [%{row: [200], occurrences: 1}]}
    )
  end

  test "aliased subquery with dots" do
    assert_query(
      "select count(height) from (select user_id, height from heights_sq) \"x.y\"",
      %{columns: ["count"], rows: [%{row: [100], occurrences: 1}]}
    )
  end

  test "[BUG] subquery star selecting identical user_id columns" do
    assert_query(
      "SELECT stddev(0) FROM (SELECT * FROM heights_sq t1 JOIN heights_sq t2 ON t1.user_id = t2.user_id) t",
      %{rows: [%{row: [0.0], occurrences: 1}]}
    )
  end
end
