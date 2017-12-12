defmodule Cloak.Query.JoinTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights_join", "height INTEGER, name TEXT, male BOOLEAN")
    :ok = Cloak.Test.DB.create_table("purchases", "price INTEGER, name TEXT")
    :ok = Cloak.Test.DB.create_table("children_join", "age INTEGER, name TEXT")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("heights_join")
    Cloak.Test.DB.clear_table("children_join")
    Cloak.Test.DB.clear_table("purchases")
    :ok
  end

  test "selecting from multiple tables" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query "select max(height), max(price)
      FROM heights_join, purchases WHERE heights_join.user_id = purchases.user_id",
      %{columns: ["max", "max"], rows: rows}
    assert [%{row: [180, 200], occurrences: 1}] = rows
  end

  test "selecting all from one table" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query "select purchases.*, height
      FROM heights_join, purchases WHERE heights_join.user_id = purchases.user_id",
      %{columns: ["user_id", "price", "name", "height"], rows: rows}

    assert [%{row: [:*, 200, nil, 180], occurrences: 101}] = rows
  end

  test "selecting all from an aliased table" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query "select p.*, height
      FROM heights_join, purchases p WHERE heights_join.user_id = p.user_id",
      %{columns: ["user_id", "price", "name", "height"], rows: rows}

    assert [%{row: [:*, 200, nil, 180], occurrences: 101}] = rows
  end

  test "multiple select all from a table" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query "select purchases.*, heights_join.*
      FROM heights_join, purchases WHERE heights_join.user_id = purchases.user_id",
      %{columns: ["user_id", "price", "name", "user_id", "height", "name", "male"], rows: rows}

    assert [%{row: [:*, 200, nil, :*, 180, nil, nil], occurrences: 101}] = rows
  end

  test "selecting from joined aliased tables" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query "select max(height), max(price)
      FROM heights_join hj, purchases p WHERE hj.user_id = p.user_id",
      %{columns: ["max", "max"], rows: rows}
    assert [%{row: [180, 200], occurrences: 1}] = rows
  end

  test "self join with an alias" do
    :ok = insert_rows(_user_ids = 1..100, "purchases", ["price"], [200])

    assert_query "select p1.price, p2.price FROM purchases p1 inner join purchases p2 on p1.user_id = p2.user_id",
      %{columns: ["price", "price"], rows: rows}
    assert [%{row: [200, 200], occurrences: 100}] = rows
  end

  test "selecting using INNER JOIN" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query """
      SELECT max(height), max(price)
      FROM heights_join INNER JOIN purchases ON heights_join.user_id = purchases.user_id
    """,
      %{columns: ["max", "max"], rows: rows}
    assert [%{row: [180, 200], occurrences: 1}] = rows
  end

  test "selecting using INNER JOIN with cast in join condition" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query """
      SELECT max(height), max(price)
      FROM heights_join
      INNER JOIN purchases ON CAST(heights_join.user_id AS TEXT) = CAST(purchases.user_id AS TEXT)
    """,
      %{columns: ["max", "max"], rows: rows}
    assert [%{row: [180, 200], occurrences: 1}] = rows
  end

  test "selecting using complex JOIN" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])
    :ok = insert_rows(_user_ids = 0..100, "children_join", ["age"], [20])

    assert_query """
      SELECT max(height), max(price), max(age)
      FROM
        heights_join INNER JOIN purchases ON heights_join.user_id = purchases.user_id,
        children_join
      WHERE children_join.user_id = purchases.user_id
    """, %{columns: ["max", "max", "max"], rows: rows}
    assert [%{row: [180, 200, 20], occurrences: 1}] = rows
  end

  test "selecting using LEFT OUTER JOIN" do
    :ok = insert_rows(_user_ids = 1..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..50, "children_join", ["age"], [20])

    assert_query """
      SELECT count(*)
      FROM heights_join LEFT OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
    """, %{columns: ["count"], rows: rows}
    assert [%{row: [100], occurrences: 1}] = rows
  end

  test "selecting using RIGHT OUTER JOIN" do
    :ok = insert_rows(_user_ids = 1..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..50, "children_join", ["age"], [20])

    assert_query """
      SELECT count(*)
      FROM heights_join RIGHT OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
    """, %{columns: ["count"], rows: rows}
    assert [%{row: [50], occurrences: 1}] = rows
  end

  test "a mistyped JOIN condition" do
    assert_query """
      SELECT count(*) FROM heights_join LEFT OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
      AND heights_join.name = children_join.age
    """, %{error: error}
    assert error == "Column `name` from table `heights_join` of type `text` and column `age` from table "
      <> "`children_join` of type `integer` cannot be compared."
  end

  test "functions in JOIN condition" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [-100])

    assert_query """
      SELECT max(height), max(price)
      FROM heights_join INNER JOIN purchases
      ON heights_join.user_id = purchases.user_id AND abs(price) = 100
    """,
      %{columns: ["max", "max"], rows: rows}
    assert [%{row: [180, -100], occurrences: 1}] = rows
  end

  test "non-selected order by in a joined table" do
    :ok = insert_rows(_user_ids = 1..50, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 51..100, "heights_join", ["height"], [170])
    :ok = insert_rows(_user_ids = 1..25, "purchases", ["price"], [200])
    :ok = insert_rows(_user_ids = 26..50, "purchases", ["price"], [50])
    :ok = insert_rows(_user_ids = 51..75, "purchases", ["price"], [150])
    :ok = insert_rows(_user_ids = 76..100, "purchases", ["price"], [100])

    assert_query "select height
      FROM heights_join INNER JOIN purchases ON heights_join.user_id = purchases.user_id
      ORDER BY price
      ",
      %{columns: ["height"], rows: rows}

    assert [
      %{row: [180], occurrences: 25},
      %{row: [170], occurrences: 25},
      %{row: [170], occurrences: 25},
      %{row: [180], occurrences: 25}
    ] = rows
  end

  test "complex joins with filters and subqueries" do
    :ok = insert_rows(_user_ids = 0..10, "purchases", ["price", "name"], [200, "car"])
    :ok = insert_rows(_user_ids = 5..15, "purchases", ["price", "name"], [500, "plane"])
    :ok = insert_rows(_user_ids = 10..20, "purchases", ["price", "name"], [400, "house"])

    assert_query """
      SELECT DISTINCT purchases.name FROM purchases INNER JOIN (
        SELECT purchases.user_id FROM purchases LEFT OUTER JOIN (
          SELECT user_id, name FROM purchases WHERE name = 'car'
        ) car ON purchases.user_id = car.user_id
        WHERE car.user_id IS NULL
        GROUP BY purchases.user_id
      ) not_car ON purchases.user_id = not_car.user_id
      ORDER BY 1 DESC
    """,
      %{rows: [%{occurrences: 1, row: ["plane"]}, %{occurrences: 1, row: ["house"]}]}
  end

  test "sample from join" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query """
      select count(*) FROM heights_join, purchases WHERE heights_join.user_id = purchases.user_id sample_users 10%
    """, %{rows: [%{row: [8], occurrences: 1}]}
  end

  test "bugfix: complex JOIN in subquery with extra identically-named columns selected" do
    :ok = insert_rows(_user_ids = 1..100, "heights_join", ["height", "name"], [180, "h"])
    :ok = insert_rows(_user_ids = 1..100, "purchases", ["price", "name"], [200, "p"])
    :ok = insert_rows(_user_ids = 1..100, "children_join", ["age", "name"], [20, "c"])

    assert_query """
      SELECT count(t1.user_id) FROM (
        SELECT h.user_id, p.name, h.name as n, bucket(age by 10) AS b
        FROM heights_join AS h
        JOIN purchases AS p ON h.user_id = p.user_id
        JOIN children_join AS c ON c.user_id = p.user_id
        WHERE height = 180 AND price = 200 AND age = 20
      ) AS t1
      JOIN children_join AS t2 ON t1.user_id = t2.user_id
    """, %{rows: [%{row: [100], occurrences: 1}]}
  end

  test "bugfix: deep join with grouping" do
    :ok = insert_rows(_user_ids = 1..15, "heights_join", ["height"], [10])
    :ok = insert_rows(_user_ids = 1..15, "purchases", ["price"], [20])

    assert_query """
      SELECT sum(x) FROM (
        SELECT t1.user_id, x1 + x2 AS x FROM
          (SELECT user_id, COUNT(*) AS x2 FROM heights_join GROUP BY user_id, height) AS t1
          INNER JOIN
          (SELECT user_id, COUNT(*) AS x1 FROM purchases GROUP BY user_id, price) AS t2
          ON t1.user_id = t2.user_id
        ) AS t
    """, %{rows: [%{row: [30], occurrences: 1}]}
  end

  test "bugfix: boolean filtering with grouping" do
    :ok = insert_rows(_user_ids = 1..10, "heights_join", ["male"], [true])
    :ok = insert_rows(_user_ids = 5..15, "heights_join", ["male"], [false])

    assert_query """
      select count(*) from
        (select t1.user_id from
          heights_join as t1 join heights_join as t2 on
          t1.user_id = t2.user_id and t1.male = t2.male
        group by 1) as t
    """, %{query_id: "1", rows: [%{row: [15]}]}
  end
end
