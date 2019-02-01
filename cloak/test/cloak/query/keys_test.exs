defmodule Cloak.Query.KeysTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok =
      Cloak.Test.DB.create_table("keys_accounts", "id INTEGER, name TEXT",
        keys: %{"user_id" => :user_id, "id" => :account_id}
      )

    for i <- 1..10, do: :ok = insert_rows("keys_accounts", ["user_id", "id", "name"], ["#{i}", i, "Jon"])

    :ok =
      Cloak.Test.DB.create_table("keys_transactions", "account_id INTEGER, product_id INTEGER",
        user_id: nil,
        add_user_id: false,
        content_type: :private,
        keys: %{"account_id" => :account_id, "product_id" => :product_id}
      )

    for i <- 1..10, do: :ok = insert_rows("keys_transactions", ["account_id", "product_id"], [i, 1])

    :ok =
      Cloak.Test.DB.create_table("keys_products", "id INTEGER, name TEXT",
        user_id: nil,
        add_user_id: false,
        content_type: :public,
        keys: %{"id" => :product_id}
      )

    :ok = insert_rows("keys_products", ["id", "name"], [1, "car"])
    :ok
  end

  test "simple select" do
    assert_query("select name, count(id) from keys_accounts group by 1", %{
      rows: [%{row: ["Jon", 10]}]
    })
  end

  test "join with private table without uid" do
    assert_query(
      """
        select product_id, count(*)
        from keys_accounts as a
        join keys_transactions as t on a.id = t.account_id
        group by 1
      """,
      %{rows: [%{row: [1, 10]}]}
    )
  end

  test "join with public table" do
    assert_query(
      """
        select p.name, count(*)
        from keys_accounts as a
        join keys_transactions as t on a.id = t.account_id
        join keys_products as p on t.product_id = p.id
        group by 1
      """,
      %{rows: [%{row: ["car", 10]}]}
    )
  end

  test "join with private table without uid and filter" do
    assert_query(
      """
        select count(*)
        from keys_accounts as a
        join keys_transactions as t on a.id = t.account_id
        where product_id = 1
      """,
      %{rows: [%{row: [10]}]}
    )
  end

  test "join with public table and filter" do
    assert_query(
      """
        select count(*)
        from keys_accounts as a
        join keys_transactions as t on a.id = t.account_id
        join keys_products as p on t.product_id = p.id
        where p.name = 'car'
      """,
      %{rows: [%{row: [10]}]}
    )
  end

  test "join and filter in subquery" do
    assert_query(
      """
        select count(*) from (
          select user_id from keys_accounts as a
          join keys_transactions as t on a.id = t.account_id
          where name = 'Jon'
        ) t
      """,
      %{rows: [%{row: [10]}]}
    )
  end

  test "join with subqueries" do
    assert_query(
      """
        select count(*) from (
          select user_id, account_id as aid, product_id as pid from
          keys_accounts as a join keys_transactions as t on a.id = t.account_id
        ) as at join (
          select id from keys_products where name = 'car'
        ) as p on pid = p.id
      """,
      %{rows: [%{row: [10]}]}
    )
  end

  test "error on missing uid in main query" do
    assert_query("select count(*) from keys_transactions", %{
      error: "Missing a `user id` key column in the tables referenced by the top-level query." <> _
    })
  end

  test "error on missing uid in subquery" do
    assert_query("select count(*) from (select product_id from keys_transactions) as t", %{
      error: "Missing a `user id` key column in the select list of subquery `t`." <> _
    })
  end

  test "error on unconnected table path" do
    assert_query(
      """
        select count(*)
        from keys_accounts as a
        join keys_transactions as t on a.id = t.account_id
        join keys_products as p on a.id = p.id
      """,
      %{error: error}
    )

    assert "The tables `a` and `p` are not joined " <> _ = error
    assert error =~ "column `id` of type `account_id` or column `user_id` of type `user_id`"
    assert error =~ "column `id` of type `product_id`"
  end

  test "anonymizing subquery" do
    assert_query(
      """
        select count(*) from (
          select count(*) from keys_accounts
          ) accounts
      """,
      %{rows: [%{row: [1]}]}
    )
  end

  test "join between anonymizing subqueries" do
    assert_query(
      """
        select count(*) from (
          select count(*) from keys_accounts
        ) accounts, (
          select distinct p.name from keys_products p
          join keys_transactions t on p.id = t.product_id
          join keys_accounts a on t.account_id = a.id
        ) purchased_products
      """,
      %{rows: [%{row: [1]}]}
    )
  end
end
