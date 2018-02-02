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
      }
    )
    :ok = Cloak.Test.DB.create_table(
      "#{@prefix}projected_emulated", "value TEXT",
      decoders: [
        %{method: "base64", columns: ["value"]},
      ],
      projection: %{
        table: "#{@prefix}main",
        foreign_key: "user_id",
        primary_key: "user_id",
        user_id_alias: "uid",
      }
    )
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("#{@prefix}main")
    Cloak.Test.DB.clear_table("#{@prefix}projected")
    Cloak.Test.DB.clear_table("#{@prefix}projected_emulated")
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}main", [], [])
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}projected", ["value"], ["a"])
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}projected", ["value"], ["b"])
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}projected_emulated", ["value"], [Base.encode64("a")])
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}projected_emulated", ["value"], [Base.encode64("b")])
    :ok
  end

  Enum.each(["projected_emulated", "projected"], fn(table) ->
    describe "simple queries on #{table}" do
      test "count(*)", do:
        assert_query "select count(*) from #{@prefix}#{unquote(table)}",
          %{rows: [%{occurrences: 1, row: [200]}]}

      test "where", do:
        assert_query "select count(value) from #{@prefix}#{unquote(table)} where value = 'a'",
          %{rows: [%{occurrences: 1, row: [100]}]}

      test "order by", do:
        assert_query "select length(value) as l from #{@prefix}#{unquote(table)} order by l desc",
          %{rows: [%{occurrences: 200, row: [1]}]}

      test "count aliased uid", do:
        assert_query "select count(distinct uid) from #{@prefix}#{unquote(table)}",
          %{rows: [%{occurrences: 1, row: [100]}]}

      test "select aliased uid", do:
        assert_query "select count(*) from (SELECT uid FROM #{@prefix}#{unquote(table)} GROUP BY uid) as l",
          %{rows: [%{occurrences: 1, row: [100]}]}
    end
  end)
end
