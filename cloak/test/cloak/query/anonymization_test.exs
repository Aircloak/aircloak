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

  describe "LIKE row dropping" do
    test "unique values with not enough users are dropped" do
      :ok = insert_rows(_user_ids = 1..10, "anonymizations", ["string"], ["alice"])
      :ok = insert_rows(_user_ids = 11..11, "anonymizations", ["string"], ["alfred"])

      assert_query "select count(*) from anonymizations where string like 'a%'",
        %{columns: ["count"], rows: [%{row: [10]}]}
    end

    test "unique values with enough users are kept" do
      :ok = insert_rows(_user_ids = 1..10, "anonymizations", ["string"], ["alice"])
      :ok = insert_rows(_user_ids = 11..15, "anonymizations", ["string"], ["alfred"])

      assert_query "select count(*) from anonymizations where string like 'a%'",
        %{columns: ["count"], rows: [%{row: [15]}]}
    end

    test "if there are many unique values nothing is dropped" do
      :ok = insert_rows(_user_ids = 1..1, "anonymizations", ["string"], ["alice"])
      :ok = insert_rows(_user_ids = 2..2, "anonymizations", ["string"], ["alfred"])
      :ok = insert_rows(_user_ids = 3..3, "anonymizations", ["string"], ["algernon"])
      :ok = insert_rows(_user_ids = 4..4, "anonymizations", ["string"], ["alma"])

      assert_query "select count(*) from anonymizations where string like 'a%'",
        %{columns: ["count"], rows: [%{row: [4]}]}
    end

    test "the whole LHS of the LIKE is considered" do
      :ok = insert_rows(_user_ids = 1..1, "anonymizations", ["string"], ["alice"])
      :ok = insert_rows(_user_ids = 2..2, "anonymizations", ["string"], ["ALICE"])
      :ok = insert_rows(_user_ids = 3..3, "anonymizations", ["string"], ["alfred"])
      :ok = insert_rows(_user_ids = 4..4, "anonymizations", ["string"], ["ALFRED"])

      assert_query "select count(*) from anonymizations where lower(string) like 'a%'",
        %{columns: ["count"], rows: [%{row: [0]}]}
    end

    test "casing is taken into account for ILIKE"
  end
end
