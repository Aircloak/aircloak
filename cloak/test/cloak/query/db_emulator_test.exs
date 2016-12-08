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

  test "simple emulated subqueries" do
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

  test "emulated subqueries with functions" do
    :ok = insert_rows(_user_ids = 1..10, "emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 11..20, "emulated", ["value"], [Base.encode64("x")])

    assert_query "select l from (select user_id, length(value) as l from emulated) as t order by l desc",
      %{rows: [%{occurrences: 10, row: [3]}, %{occurrences: 10, row: [1]}]}
    assert_query "select value from (select user_id, left(value, 1) as value from emulated) as t where value = 'a'",
      %{rows: [%{occurrences: 10, row: ["a"]}]}
  end

  test "aggregation in emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("x")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("xyx")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("abcde")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("1234")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [nil])

    assert_query "select avg(v) from (select user_id, count(*) as v from emulated group by user_id) as t",
      %{rows: [%{occurrences: 1, row: [6.0]}]}

    assert_query "select avg(v) from (select user_id, count(value) as v from emulated group by user_id) as t",
      %{rows: [%{occurrences: 1, row: [5.0]}]}

    assert_query """
      select v from
        (select user_id, left(value, 1) as v from emulated group by user_id, left(value, 1)) as t
      order by v
      """, %{rows: [%{occurrences: 20, row: [""]}, %{occurrences: 20, row: ["1"]},
            %{occurrences: 20, row: ["a"]}, %{occurrences: 20, row: ["x"]}]}

    assert_query """
      select length(v) as v from
        (select user_id, left(value, 1) as v from emulated group by user_id, value having length(value) > 1) as t
      order by v
      """, %{rows: [%{occurrences: 80, row: [1]}]}

    assert_query """
        select avg(v) from
          (select user_id, sum(length(value)) as v from emulated group by user_id having max(length(value)) = 5) as t
      """, %{rows: [%{occurrences: 1, row: [16.0]}]}
    assert_query """
        select v from (select user_id, min(length(value)) as v from emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [1]}]}
    assert_query """
        select v from (select user_id, max(length(value)) as v from emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [5]}]}
    assert_query """
        select round(avg(v)) from (select user_id, avg(length(value)) as v from emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3]}]}
    assert_query """
        select round(avg(v)) from (select user_id, stddev(length(value)) as v from emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [2]}]}
    assert_query """
        select round(avg(v)) from (select user_id, median(length(value)) as v from emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3]}]}
  end

  test "distinct in emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("x")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("xyx")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("abcde")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [Base.encode64("1234")])
    :ok = insert_rows(_user_ids = 1..20, "emulated", ["value"], [nil])

    assert_query "select avg(v) from (select user_id, count(distinct value) as v from emulated group by user_id) as t",
      %{rows: [%{occurrences: 1, row: [5.0]}]}

    assert_query """
        select avg(v) from (select user_id, count(distinct left(value, 1)) as v from emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [4.0]}]}

    assert_query "select v from (select distinct user_id, length(value) as v from emulated) as t",
      %{rows: [
        %{occurrences: 20, row: [1]}, %{occurrences: 20, row: [3]}, %{occurrences: 20, row: [4]},
        %{occurrences: 20, row: [5]}, %{occurrences: 20, row: [nil]}
      ]}

    assert_query """
        select avg(v) from (select user_id, avg(distinct length(value)) as v from emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3.25]}]}
    end
end
