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

  test "selecting using FULL OUTER JOIN" do
    :ok = insert_rows(_user_ids = 1..50, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 101..150, "children_join", ["age"], [20])

    assert_query """
      SELECT count(*)
      FROM heights_join FULL OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
    """, %{columns: ["count"], rows: rows}
    assert [%{row: [100], occurrences: 1}] = rows
  end

  test "a mistyped JOIN condition" do
    assert_query """
      SELECT count(*) FROM heights_join FULL OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
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
end
