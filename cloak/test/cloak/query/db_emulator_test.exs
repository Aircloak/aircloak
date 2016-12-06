defmodule Cloak.Query.DBEmulatorTest do
  use ExUnit.Case, async: false

  import Cloak.Test.QueryHelpers

  setup_all do
    decoder = %{method: "base64", columns: ["value"]}
    :ok = Cloak.Test.DB.create_table("emulated", "value TEXT", [decoders: [decoder]])
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("emulated")
    :ok
  end

  test "execution of simple emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..10, "emulated", ["value"], [Base.encode64("aaa")])
    :ok = insert_rows(_user_ids = 11..20, "emulated", ["value"], [Base.encode64("bbb")])
    :ok = insert_rows(_user_ids = 21..30, "emulated", ["value"], [nil])

    assert_query "select count(value) from (select user_id, value from emulated where value = 'aaa') as t",
      %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query "select count(value) from (select user_id, value from emulated where value is not null) as t",
      %{rows: [%{occurrences: 1, row: [20]}]}
    assert_query "select count(*) from (select user_id, value from emulated order by value limit 10) as t",
      %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query "select count(*) from (select user_id, value from emulated order by value limit 10 offset 10) as t",
      %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "execution of emulated subqueries with functions" do
    :ok = insert_rows(_user_ids = 1..10, "emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 11..20, "emulated", ["value"], [Base.encode64("x")])

    assert_query "select l from (select user_id, length(value) as l from emulated) as t order by l desc",
      %{rows: [%{occurrences: 10, row: [3]}, %{occurrences: 10, row: [1]}]}
    assert_query "select value from (select user_id, left(value, 1) as value from emulated) as t where value = 'a'",
      %{rows: [%{occurrences: 10, row: ["a"]}]}
  end
end
