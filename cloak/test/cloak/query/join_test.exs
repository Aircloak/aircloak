defmodule Cloak.Query.JoinTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights_join", "height INTEGER, name TEXT, male BOOLEAN")
    :ok = Cloak.Test.DB.create_table("purchases", "price INTEGER, name TEXT, datetime TIMESTAMP")
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
    assert rows == [%{row: [180, 200], occurrences: 1}]
  end

  test "selecting using INNER JOIN" do
    :ok = insert_rows(_user_ids = 0..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 0..100, "purchases", ["price"], [200])

    assert_query """
      SELECT max(height), max(price)
      FROM heights_join INNER JOIN purchases ON heights_join.user_id = purchases.user_id
    """,
      %{columns: ["max", "max"], rows: rows}
    assert rows == [%{row: [180, 200], occurrences: 1}]
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
    assert rows == [%{row: [180, 200, 20], occurrences: 1}]
  end

  test "selecting using LEFT OUTER JOIN" do
    :ok = insert_rows(_user_ids = 1..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..50, "children_join", ["age"], [20])

    assert_query """
      SELECT count(*)
      FROM heights_join LEFT OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
    """, %{columns: ["count"], rows: rows}
    assert rows == [%{row: [100], occurrences: 1}]
  end

  test "selecting using RIGHT OUTER JOIN" do
    :ok = insert_rows(_user_ids = 1..100, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..50, "children_join", ["age"], [20])

    assert_query """
      SELECT count(*)
      FROM heights_join RIGHT OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
    """, %{columns: ["count"], rows: rows}
    assert rows == [%{row: [50], occurrences: 1}]
  end

  test "selecting using FULL OUTER JOIN" do
    :ok = insert_rows(_user_ids = 1..50, "heights_join", ["height"], [180])
    :ok = insert_rows(_user_ids = 101..150, "children_join", ["age"], [20])

    assert_query """
      SELECT count(*)
      FROM heights_join FULL OUTER JOIN children_join ON heights_join.user_id = children_join.user_id
    """, %{columns: ["count"], rows: rows}
    assert rows == [%{row: [100], occurrences: 1}]
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
    assert rows == [%{row: [180, -100], occurrences: 1}]
  end
end
