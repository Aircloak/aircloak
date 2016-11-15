defmodule Cloak.Query.AnonymizationTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("anonymizations", "number INTEGER")
  end

  setup do
    Cloak.Test.DB.clear_table("anonymizations")
    :ok
  end

  test "shrink and drop" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [6])
    :ok = insert_rows(_user_ids = 0..0, "anonymizations", ["number"], [2])
    assert_query "select count(*) from anonymizations where number >= 0 and number < 10",
      %{columns: ["count"], rows: [%{row: [100]}]}
  end

  test "recursive shrink and drop" do
    :ok = insert_rows(_user_ids = 1..100, "anonymizations", ["number"], [6])
    :ok = insert_rows(_user_ids = 0..0, "anonymizations", ["number"], [8])
    assert_query "select count(*) from anonymizations where number >= 0 and number < 20",
      %{columns: ["count"], rows: [%{row: [100]}]}
  end
end
