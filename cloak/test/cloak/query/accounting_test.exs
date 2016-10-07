defmodule Cloak.Query.AccountingTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("heights", "height INTEGER, name TEXT, male BOOLEAN")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("heights")
    :ok
  end

  test "account for the number of users processed" do
    :ok = insert_rows(_user_ids = 1..50, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 51..100, "heights", ["height"], [190])
    assert_query "select height from heights", %{au_count: 100}
  end

  test "only count each user once" do
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [180])
    :ok = insert_rows(_user_ids = 1..100, "heights", ["height"], [190])
    assert_query "SELECT height FROM heights WHERE height IN (180, 190)", %{au_count: 100}
  end
end
