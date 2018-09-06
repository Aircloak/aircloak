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

  test "emulated function in over userless table" do
    assert_query("select median(i) from userless", %{
      rows: [%{row: [2]}]
    })
  end

  test "join between userless tables" do
    assert_query(
      "select t1.name from userless as t1 join userless as t2 on t1.i = t2.i and t1.name <> t2.name order by 1",
      %{rows: [%{row: ["drinks"]}, %{row: ["food"]}]}
    )
  end

  test "join between user and userless tables" do
    assert_query(
      "select count(*) from userless as t1 join userless_join as t2 on t1.i = t2.i",
      %{error: "Table/subquery `t1` has no associated user id. Userless data can not be used in anonymizing queries."}
    )
  end

  test "join between userless tables with inequality filter in the ON clause" do
    assert_query(
      "select count(*) from userless as t1 join userless as t2 on false < true",
      %{rows: [%{row: [16]}]}
    )
  end

  test "join between anonymizing and standard query with constant equality filter in the ON clause" do
    assert_query(
      """
        select * from
          (select count(*) from userless) as t1
        join
          (select count(*) from userless_join) as t2
        on true = true
      """,
      %{rows: [%{row: [4, 30]}]}
    )
  end

  test "standard query with sample users" do
    assert_query(
      "select name from userless sample_users 10%",
      %{error: "The `SAMPLE_USERS` clause is not valid in standard queries."}
    )
  end
end
