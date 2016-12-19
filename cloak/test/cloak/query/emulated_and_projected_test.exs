defmodule Cloak.Query.EmulatedAndProjectedTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  @prefix "emulated_and_projected_"

  setup_all do
    :ok = Cloak.Test.DB.create_table("#{@prefix}main", "dummy BOOLEAN")
    decoder = %{method: "base64", columns: ["value"]}
    projection = %{table: "#{@prefix}main", foreign_key: "user_id", primary_key: "user_id"}
    :ok = Cloak.Test.DB.create_table("#{@prefix}emulated", "value TEXT", decoders: [decoder], projection: projection)
    :ok = Cloak.Test.DB.create_table("#{@prefix}joined", "age INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("#{@prefix}emulated")
    Cloak.Test.DB.clear_table("#{@prefix}main")
    Cloak.Test.DB.clear_table("#{@prefix}joined")
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}main", [], [])
    :ok
  end

  test "simple emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["value"], [Base.encode64("aaa")])
    :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated", ["value"], [Base.encode64("bbb")])
    :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated", ["value"], [nil])

    assert_query "select count(value) from (select user_id, value from #{@prefix}emulated where value = 'aaa') as t",
      %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query """
        select count(value) from (select user_id, value from #{@prefix}emulated where value is not null) as t
      """, %{rows: [%{occurrences: 1, row: [20]}]}
    assert_query "select count(*) from (select user_id, value from #{@prefix}emulated order by value limit 10) as t",
      %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query """
        select count(*) from (select user_id, value from #{@prefix}emulated order by value limit 10 offset 10) as t
      """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "emulated subqueries with functions" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated", ["value"], [Base.encode64("x")])

    assert_query "select l from (select user_id, length(value) as l from #{@prefix}emulated) as t order by l desc",
      %{rows: [%{occurrences: 10, row: [3]}, %{occurrences: 10, row: [1]}]}
    assert_query """
        select value from (select user_id, left(value, 1) as value from #{@prefix}emulated) as t where value = 'a'
      """, %{rows: [%{occurrences: 10, row: ["a"]}]}
  end

  test "aggregation in emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("x")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("xyx")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("abcde")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("1234")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [nil])

    assert_query "select avg(v) from (select user_id, count(*) as v from #{@prefix}emulated group by user_id) as t",
      %{rows: [%{occurrences: 1, row: [6.0]}]}

    assert_query "select avg(v) from (select user_id, count(value) as v from #{@prefix}emulated group by user_id) as t",
      %{rows: [%{occurrences: 1, row: [5.0]}]}

    assert_query """
      select v from
        (select user_id, left(value, 1) as v from #{@prefix}emulated group by user_id, left(value, 1)) as t
      order by v
      """, %{rows: [%{occurrences: 20, row: [""]}, %{occurrences: 20, row: ["1"]},
            %{occurrences: 20, row: ["a"]}, %{occurrences: 20, row: ["x"]}]}

    assert_query """
      select length(v) as v from
        (select user_id, left(value, 1) as v from #{@prefix}emulated
        group by user_id, value having length(value) >= 1 and length(value) < 2) as t
      order by v
      """, %{rows: [%{occurrences: 20, row: [1]}]}

    assert_query """
        select avg(v) from
          (select user_id, sum(length(value)) as v from #{@prefix}emulated
          group by user_id having max(length(value)) = 5) as t
      """, %{rows: [%{occurrences: 1, row: [16.0]}]}
    assert_query """
        select v from (select user_id, min(length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [1]}]}
    assert_query """
        select v from (select user_id, max(length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [5]}]}
    assert_query """
        select round(avg(v)) from
        (select user_id, avg(length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3]}]}
    assert_query """
        select round(avg(v)) from
        (select user_id, stddev(length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [1]}]}
    assert_query """
        select round(avg(v)) from
        (select user_id, median(length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3]}]}
  end

  test "distinct in emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("x")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("xyx")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("abcde")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("1234")])
    :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [nil])

    assert_query """
        select avg(v) from (select user_id, count(distinct value) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [5.0]}]}

    assert_query """
        select avg(v) from
        (select user_id, count(distinct left(value, 1)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [4.0]}]}

    assert_query "select v from (select distinct user_id, length(value) as v from #{@prefix}emulated) as t",
      %{rows: [
        %{occurrences: 20, row: [1]}, %{occurrences: 20, row: [3]}, %{occurrences: 20, row: [4]},
        %{occurrences: 20, row: [5]}, %{occurrences: 20, row: [nil]}
      ]}

    assert_query """
        select avg(v) from
        (select user_id, avg(distinct length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3.25]}]}
  end

  describe "emulated joins" do

    defp join_setup(_) do
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("abc")])
      :ok = insert_rows(_user_ids = 11..25, "#{@prefix}joined", ["age"], [30])
    end

    setup [:join_setup]

    test "cross join", do:
      assert_query """
          select count(age) from
          #{@prefix}emulated, #{@prefix}joined where #{@prefix}emulated.user_id = #{@prefix}joined.user_id
        """, %{rows: [%{occurrences: 1, row: [10]}]}

    test "inner join", do:
      assert_query """
          select count(*) from
          #{@prefix}emulated inner join (select user_id as uid from #{@prefix}joined) as t on user_id = uid
        """, %{rows: [%{occurrences: 1, row: [10]}]}

    test "left join between table and subquery" do
      assert_query """
          select count(*) from
          #{@prefix}emulated left join (select user_id as uid from #{@prefix}joined) as t on user_id = uid
        """, %{rows: [%{occurrences: 1, row: [20]}]}

      assert_query """
          select count(age) from
          #{@prefix}emulated left join #{@prefix}joined
          on #{@prefix}emulated.user_id = #{@prefix}joined.user_id where age = 30
        """, %{rows: [%{occurrences: 1, row: [10]}]}
    end

    test "right join", do:
      assert_query """
          select count(*) from
          #{@prefix}emulated right join #{@prefix}joined
          on #{@prefix}emulated.user_id = #{@prefix}joined.user_id
        """, %{rows: [%{occurrences: 1, row: [15]}]}

    test "full join" do
      assert_query """
          select count(*) from
            (select user_id as uid1 from #{@prefix}emulated) as t
            full join
            (select user_id as uid2 from #{@prefix}joined) as t2
            on uid1 = uid2
        """, %{rows: [%{occurrences: 1, row: [25]}]}

      assert_query """
          select count(x) from
            (select user_id as uid1 from #{@prefix}emulated) as t
            full join
            (select user_id as uid2, age as x from #{@prefix}joined) as t2
            on uid1 = uid2
        """, %{rows: [%{occurrences: 1, row: [15]}]}
    end
  end
end
