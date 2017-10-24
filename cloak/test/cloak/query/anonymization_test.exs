defmodule Cloak.Query.AnonymizationTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("anonymizations", "number REAL, string TEXT")
    :ok = Cloak.Test.DB.create_table("other_table", "dummy BOOLEAN")
  end

  setup do
    Cloak.Test.DB.clear_table("anonymizations")
    Cloak.Test.DB.clear_table("other_table")
    :ok
  end

  describe "count(distinct)" do
    setup do
      :ok = insert_rows(_user_ids = 0..19, "anonymizations", ["number"], [180])
      :ok = insert_rows(_user_ids = 20..29, "anonymizations", ["number"], [170])
      :ok = insert_rows(_user_ids = 20..29, "anonymizations", ["number"], [175])
      :ok = insert_rows(_user_ids = 30..39, "anonymizations", ["number"], [160])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [150])
      :ok = insert_rows(_user_ids = 50..59, "anonymizations", ["number"], [190])

      :ok
    end

    test "counting values represented by many users" do
      assert_query "select count(distinct number) from anonymizations",
        %{columns: ["count"], rows: [%{row: [5], occurrences: 1}]}
    end

    test "ignoring nils" do
      :ok = insert_rows(_user_ids = 41..49, "anonymizations", ["number"], [nil])

      assert_query "select count(distinct number) from anonymizations",
        %{columns: ["count"], rows: [%{row: [5], occurrences: 1}]}
    end

    test "hiding users with many distinct values" do
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [151])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [152])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [153])

      assert_query "select count(distinct number) from anonymizations",
        %{columns: ["count"], rows: [%{row: [5], occurrences: 1}]}
    end

    test "a user with many non-unique values should be treated as one with few distinct values" do
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [151])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [152])
      :ok = insert_rows(_user_ids = 40..40, "anonymizations", ["number"], [153])
      for _ <- 1..10, do:
        :ok = insert_rows(_user_ids = [60], "anonymizations", ["number"], [151])

      assert_query "select count(distinct number) from anonymizations",
        %{columns: ["count"], rows: [%{row: [6], occurrences: 1}]}
    end
  end
end
