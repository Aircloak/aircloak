defmodule Cloak.Query.AccountingTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("numerics", "number INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("numerics")
    :ok
  end

  test "account for the number of users processed" do
    :ok = insert_rows(_user_ids = 1..50, "numerics", ["number"], [1])
    :ok = insert_rows(_user_ids = 51..100, "numerics", ["number"], [2])
    assert_query "select number from numerics", %{au_count: 100}
  end

  test "only count each user once" do
    :ok = insert_rows(_user_ids = 1..100, "numerics", ["number"], [1])
    :ok = insert_rows(_user_ids = 1..100, "numerics", ["number"], [2])
    assert_query "SELECT number FROM numerics WHERE number IN (1, 2)", %{au_count: 100}
  end
end
