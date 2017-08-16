defmodule Cloak.Query.AliasedProjectionTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  @prefix "aliased_and_projected_"

  setup_all do
    :ok = Cloak.Test.DB.create_table("#{@prefix}main", "active BOOLEAN")
    :ok = Cloak.Test.DB.create_table(
      "#{@prefix}projected", "value TEXT",
      projection: %{
        table: "#{@prefix}main",
        foreign_key: "user_id",
        primary_key: "user_id",
        user_id_alias: "uid",
      },
    )
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("#{@prefix}projected")
    Cloak.Test.DB.clear_table("#{@prefix}main")
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}main", [], [])
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}projected", ["value"], ["a"])
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}projected", ["value"], ["b"])
    :ok
  end

  describe "simple queries" do
    test "count(*)", do:
      assert_query "select count(*) from #{@prefix}projected",
        %{rows: [%{occurrences: 1, row: [200]}]}

    test "where", do:
      assert_query "select count(value) from #{@prefix}projected where value = 'a'",
        %{rows: [%{occurrences: 1, row: [100]}]}

    test "order by", do:
      assert_query "select length(value) as l from #{@prefix}projected order by l desc",
        %{rows: [%{occurrences: 200, row: [1]}]}

    test "count aliased uid", do:
      assert_query "select count(distinct uid) from #{@prefix}projected",
        %{rows: [%{occurrences: 1, row: [100]}]}

    test "select aliased uid", do:
      assert_query "select count(*) from (SELECT uid FROM #{@prefix}projected GROUP BY uid) as l",
        %{rows: [%{occurrences: 1, row: [100]}]}
  end
end
