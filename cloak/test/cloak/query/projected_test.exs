defmodule Cloak.Query.ProjectedTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    Cloak.Test.DB.create_table("projected_accounts", "id INTEGER, name TEXT")
    Cloak.Test.DB.create_table("projected_heights", "height integer")
    Cloak.Test.DB.create_table("projected_transactions", "id INTEGER, account_id INTEGER, amount INTEGER",
      add_user_id: false, user_id: "id",
      projection: %{table: "projected_accounts", foreign_key: "account_id", primary_key: "id"}
    )
    Cloak.Test.DB.create_table("projected_notes", "transaction_id INTEGER, note TEXT",
      add_user_id: false,
      projection: %{table: "projected_transactions", foreign_key: "transaction_id", primary_key: "id"}
    )

    Enum.each(1..10, &insert_rows(&1..&1, "projected_accounts", ["id", "name"], [&1, "account_#{&1}"]))
    Enum.each(1..10, &insert_transaction(&1, 100, "note text"))
    insert_rows(_user_ids = 1..100, "projected_heights", ["height"], [180])
  end

  test "projected tables are included in show tables" do
    assert_query "show tables", %{rows: rows}
    assert(Enum.any?(rows, &(hd(&1.row) == "projected_transactions")))
    assert(Enum.any?(rows, &(hd(&1.row) == "projected_notes")))
  end

  test "show columns from a projected table" do
    assert_query "show columns from projected_transactions",
      %{rows: [
        %{row: ["user_id", :text]},
        %{row: ["id", :integer]},
        %{row: ["account_id", :integer]},
        %{row: ["amount", :integer]}
      ]}
  end

  test "show columns from a multiply projected table" do
    assert_query "show columns from projected_notes",
      %{rows: [
        %{row: ["user_id", :text]},
        %{row: ["transaction_id", :integer]},
        %{row: ["note", :text]}
      ]}
  end

  test "selecting from a projected table" do
    assert_query "select amount from projected_transactions",
      %{columns: ["amount"], rows: [%{row: [100], occurrences: 10}]}
  end

  test "selecting from an aliased projected table" do
    assert_query "select pt.amount from projected_transactions pt",
      %{columns: ["amount"], rows: [%{row: [100], occurrences: 10}]}
  end

  test "selecting from a multiply projected table" do
    assert_query "select note from projected_notes",
      %{columns: ["note"], rows: [%{row: ["note text"], occurrences: 10}]}
  end

  test "expression in a projected table" do
    assert_query "select abs(amount) from projected_transactions",
      %{columns: ["abs"], rows: [%{row: [100], occurrences: 10}]}
  end

  test "projected table in a subquery" do
    assert_query "select amount from (select user_id, amount from projected_transactions) sq_alias",
      %{columns: ["amount"], rows: [%{row: [100], occurrences: 10}]}
  end

  test "expression in a subquery using a projected table" do
    assert_query "select x from (select user_id, abs(amount) as x from projected_transactions) t",
      %{columns: ["x"], rows: [%{row: [100], occurrences: 10}]}

    assert_query "select x from (select user_id, sqrt(abs(amount)) as x from projected_transactions) t",
      %{columns: ["x"], rows: [%{row: [10.0], occurrences: 10}]}
  end

  test "joining to a projected table" do
    assert_query(
      "
        select height, amount
        from projected_heights
        inner join projected_transactions on projected_heights.user_id = projected_transactions.user_id
      ",
      %{columns: ["height", "amount"], rows: [%{row: [180, 100], occurrences: 10}]}
    )

    assert_query(
      "
        select height, amount
        from projected_transactions
        inner join projected_heights on projected_heights.user_id = projected_transactions.user_id
      ",
      %{columns: ["height", "amount"], rows: [%{row: [180, 100], occurrences: 10}]}
    )
  end

  defp insert_transaction(id, amount, note) do
    Cloak.Test.DB.insert_data("projected_transactions", ["id", "account_id", "amount"], [[id, id, amount]])
    Cloak.Test.DB.insert_data("projected_notes", ["transaction_id", "note"], [[id, note]])
  end
end
