defmodule Cloak.Query.UserlessTableTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("userless", "i INTEGER, name TEXT", user_id: nil)
    :ok = insert_rows(_user_ids = [0], "userless", ["i", "name"], [1, "car"])
    :ok = insert_rows(_user_ids = [0], "userless", ["i", "name"], [2, "food"])
    :ok = insert_rows(_user_ids = [0], "userless", ["i", "name"], [2, "drinks"])
    :ok = insert_rows(_user_ids = [0], "userless", ["i", "name"], [3, "fun"])

    :ok = Cloak.Test.DB.create_table("userless_join", "i INTEGER")
    :ok = insert_rows(_user_ids = 1..10, "userless_join", ["i"], [1])
    :ok = insert_rows(_user_ids = 11..20, "userless_join", ["i"], [2])
    :ok = insert_rows(_user_ids = 21..30, "userless_join", ["i"], [3])

    :ok
  end

  test "simple select" do
    assert_query("select name from userless order by name", %{
      rows: [%{row: ["car"]}, %{row: ["drinks"]}, %{row: ["food"]}, %{row: ["fun"]}]
    })
  end

  test "select with filter" do
    assert_query("select name from userless where name in ('fun', 'car') order by name", %{
      rows: [%{row: ["car"]}, %{row: ["fun"]}]
    })
  end

  test "select from subquery" do
    assert_query("select name from (select name from userless order by name limit 2 offset 1) as t", %{
      rows: [%{row: ["drinks"]}, %{row: ["food"]}]
    })
  end

  test "join between userless tables" do
    assert_query(
      """
        select t1.name from userless as t1 join userless as t2 on t1.i = t2.i and t1.name <> t2.name order by 1
      """,
      %{
        rows: [%{row: ["drinks"]}, %{row: ["food"]}]
      }
    )
  end

  test "join between user and userless tables" do
    assert_query(
      """
        select count(*) from userless as t1 join userless_join as t2 on t1.i = t2.i
      """,
      %{
        rows: [%{row: [40]}]
      }
    )
  end
end
