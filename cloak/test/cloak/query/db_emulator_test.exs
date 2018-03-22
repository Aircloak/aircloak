defmodule Cloak.Query.DBEmulatorTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  @prefix "db_emulator_"

  setup_all do
    :ok = Cloak.Test.DB.create_table("#{@prefix}emulated_real", "value TEXT, date TEXT, number INTEGER")
    :ok = Cloak.Test.DB.create_table("#{@prefix}emulated", nil, skip_db_create: true, query: """
      SELECT user_id, dec_b64(value) as decoded_value, date, number FROM cloak_test.#{@prefix}emulated_real
    """)
    :ok = Cloak.Test.DB.create_table("#{@prefix}joined", "age INTEGER")
    :ok
  end

  setup do

    Cloak.Test.DB.clear_table("#{@prefix}emulated_real")
    Cloak.Test.DB.clear_table("#{@prefix}joined")
    :ok
  end

  test "simple emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["value"], [Base.encode64("aaa")])
    :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated_real", ["value"], [Base.encode64("bbb")])
    :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated_real", ["value"], [nil])

    assert_query """
      select count(v) from (select user_id, decoded_value AS v from #{@prefix}emulated where v = 'aaa') as t
    """, %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query """
      select count(v) from (select user_id, decoded_value AS v from #{@prefix}emulated where v is not null) as t
    """, %{rows: [%{occurrences: 1, row: [20]}]}
    assert_query """
      select count(*) from (select user_id from #{@prefix}emulated order by decoded_value limit 10) as t
    """, %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query """
      select count(*) from (select user_id from #{@prefix}emulated order by decoded_value limit 10 offset 10) as t
    """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "emulated subqueries with functions" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated_real", ["value"], [Base.encode64("x")])

    assert_query """
      select l from (select user_id, length(decoded_value) as l from #{@prefix}emulated) as t order by l desc
    """, %{rows: [%{occurrences: 10, row: [3]}, %{occurrences: 10, row: [1]}]}
    assert_query """
      select v from (select user_id, decoded_value as v from #{@prefix}emulated) as t where length(v) = 1
    """, %{rows: [%{occurrences: 10, row: ["x"]}]}
    assert_query """
      select v from (select user_id, decoded_value AS v from #{@prefix}emulated where length(v) = 3) as t
    """, %{rows: [%{occurrences: 10, row: ["abc"]}]}
  end

  test "like on an emulated column" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["value"], [Base.encode64("aba")])
    :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated_real", ["value"], [Base.encode64("aca")])
    :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated_real", ["value"], [Base.encode64("bbb")])

    assert_query "select count(*) from #{@prefix}emulated where decoded_value like 'a_a%'", %{rows: [%{row: [20]}]}
  end

  describe "aggregation in emulated subqueries" do
    setup do
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("abc"), "2016-11-02"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("x"), "2015-01-30"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("xyz"), "2014-02-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("abcde"), "2013-02-08"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("1234"), "2013-12-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [nil, nil])
    end

    test "count(*)" do
      assert_query "select avg(v) from (select user_id, count(*) as v from #{@prefix}emulated group by user_id) as t",
        %{rows: [%{occurrences: 1, row: [6.0]}]}
    end

    test "count(<column>)" do
      assert_query """
        select avg(v) from (select user_id, count(decoded_value) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [5.0]}]}
    end

    test "group by function" do
      assert_query """
        select v from
          (select user_id, decoded_value as v from #{@prefix}emulated group by user_id, v) as t
        order by v
      """, %{rows: [%{occurrences: 20, row: ["1234"]}, %{occurrences: 20, row: ["abc"]},
        %{occurrences: 20, row: ["abcde"]}, %{occurrences: 20, row: ["x"]},
        %{occurrences: 20, row: ["xyz"]}, %{occurrences: 20, row: [nil]}]}
    end

    test "where function" do
      assert_query """
          select avg(v) from (select user_id, count(decoded_value) as v from
          #{@prefix}emulated where length(decoded_value) = 3 group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [2.0]}]}
    end

    test "having inequality" do
      :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["number"], [3])
      :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated_real", ["number"], [7])
      :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated_real", ["number"], [30])

      assert_query """
        select n + c from
          (select user_id, number + 2 as n, count(decoded_value) as c from #{@prefix}emulated
          group by user_id, number having number >= 0 and number < 10) as t
        order by n + c
      """, %{rows: [%{occurrences: 10, row: [5]}, %{occurrences: 10, row: [9]}]}
    end

    test "nested having inequality" do
      :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["number"], [3])
      :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated_real", ["number"], [7])
      :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated_real", ["number"], [30])

      assert_query """
        select n+c from
          (select user_id, n, c from
            (select user_id, number + 2 as n, count(decoded_value) as c from #{@prefix}emulated
            group by user_id, number having number >= 0 and number < 10) as t
          group by user_id, n, c) as foo
        order by n
      """, %{rows: [%{occurrences: 10, row: [5]}, %{occurrences: 10, row: [9]}]}
    end

    test "where inequality" do
      assert_query """
        select sum(c) from
          (select user_id, count(decoded_value) as c from #{@prefix}emulated
          where cast(date as date) >= '2015-01-01' and cast(date as date) < '2016-01-01'
          group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [20]}]}
    end

    test "nested where inequality" do
      assert_query """
        select sum(c) from
          (select user_id, c from
            (select user_id, count(decoded_value) as c from #{@prefix}emulated
            where cast(date as date) >= '2015-01-01' and cast(date as date) < '2016-01-01'
            group by user_id) as t
          group by user_id, c) as foo
      """, %{rows: [%{occurrences: 1, row: [20]}]}
    end

    test "having equality" do
      assert_query """
        select avg(v) from
          (select user_id, sum(length(decoded_value)) as v from #{@prefix}emulated
          group by user_id having max(length(decoded_value)) = 5) as t
      """, %{rows: [%{occurrences: 1, row: [16.0]}]}
    end

    test "avg" do
      assert_query """
        select round(avg(v)) from
          (select user_id, avg(length(decoded_value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3]}]}
    end

    test "stddev" do
      :ok = insert_rows(_user_ids = 21..21, "#{@prefix}emulated_real", ["value"], [Base.encode64("wasabi")])
      assert_query """
        select round(avg(v)) from
          (select user_id, stddev(length(decoded_value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [1]}]}
    end

    test "min/max/median with numbers" do
      assert_query """
        select * from (
          select user_id, min(length(decoded_value)), max(length(decoded_value)), median(length(decoded_value))
          from #{@prefix}emulated group by user_id
        ) as t
      """, %{rows: [%{occurrences: 20, row: [:*, 1, 5, 3]}]}
    end

    test "min/max/median with text" do
      assert_query """
        select * from (select user_id, min(decoded_value), max(decoded_value), median(decoded_value)
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "1234", "xyz", "abcde"]}]}
    end

    test "min/max/median with date" do
      assert_query """
        select * from (select user_id, min(cast(date as date)),
            max(cast(date as date)), median(cast(date as date)), count(decoded_value) as c
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "2013-02-08", "2016-11-02", "2014-02-04", 5]}]}
    end

    test "selecting a grouped column under an alias" do
      assert_query """
        select * from (
          select user_id, decoded_value as v, decoded_value as alias
          from #{@prefix}emulated where v = 'x' group by user_id, v
        ) foo
      """, %{rows: [%{occurrences: 20, row: [:*, "x", "x"]}]}
    end
  end

  describe "distinct in emulated subqueries" do
    setup do
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("abc"), "2016-11-02"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("x"), "2015-01-30"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("xyz"), "2014-02-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("abcde"), "2013-02-08"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [Base.encode64("1234"), "2013-12-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real",
        ["value", "date"], [nil, nil])
    end

    test "count(distinct value)", do:
      assert_query """
        select avg(v) from (select user_id, count(distinct decoded_value) as v
        from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [5.0]}]}

    test "count(distinct length(value))", do:
      assert_query """
        select avg(v) from
          (select user_id, count(distinct length(decoded_value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [4.0]}]}

    test "select distinct", do:
      assert_query "select v from (select distinct user_id, length(decoded_value) as v from #{@prefix}emulated) as t",
        %{rows: [
          %{occurrences: 20, row: [1]}, %{occurrences: 20, row: [3]}, %{occurrences: 20, row: [4]},
          %{occurrences: 20, row: [5]}, %{occurrences: 20, row: [nil]}
        ]}

    test "avg(distinct)", do:
      assert_query """
        select avg(v) from
          (select user_id, avg(distinct length(decoded_value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3.25]}]}

    test "distinct min/max/median with text" do
      assert_query """
        select * from (select user_id,
          min(distinct decoded_value), max(distinct decoded_value), median(distinct decoded_value)
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "1234", "xyz", "abcde"]}]}
    end

    test "distinct min/max/median with date" do
      assert_query """
        select * from (select user_id, min(distinct cast(date as date)),
          max(distinct cast(date as date)), median(distinct cast(date as date)), count(decoded_value) as c
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "2013-02-08", "2016-11-02", "2014-02-04", 5]}]}
    end
  end

  describe "emulated joins" do
    setup do
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated_real", ["value"], [Base.encode64("a b c")])
      :ok = insert_rows(_user_ids = 11..25, "#{@prefix}joined", ["age"], [30])
    end

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
        select count(decoded_value) from
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

    test "join with a row splitter function" do
      assert_query """
        select extract_words(value) from #{@prefix}joined inner join
          (select user_id as uid, decoded_value as value from #{@prefix}emulated) as t on user_id = uid
      """, %{rows: rows}

      assert length(rows) == 3
      Enum.zip(["a", "b", "c"], rows)
      |> Enum.each(fn({value, row}) -> assert %{occurrences: 10, row: [^value]} = row end)
    end

    test "where inequality" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated_real", ["number"], [3])

      assert_query """
        select count(*) - count(x.v) from
          (select user_id, decoded_value as v from #{@prefix}emulated
            where number >= 0 and number < 10 group by user_id, v) x
          inner join
          (select user_id from #{@prefix}joined group by user_id) y
          on x.user_id = y.user_id
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "nested where inequality" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated_real", ["number"], [3])

      assert_query """
        select count(c) from
          (select user_id, c from
            (select user_id, count(decoded_value) as c from #{@prefix}emulated
              where number >= 0 and number < 10 group by user_id) z
          group by user_id, c) x
          inner join
          (select user_id from #{@prefix}joined group by user_id) y
          on x.user_id = y.user_id
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "range in a join condition" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated_real", ["number"], [3])

      assert_query """
        select count(*) - count(v) from
          (select user_id, number, decoded_value as v from #{@prefix}emulated) as t
          join #{@prefix}joined
          on t.user_id = #{@prefix}joined.user_id and t.number >= 0 and t.number < 10
          where number = 3
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "range in a nested join condition" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated_real", ["number"], [3])

      assert_query """
        select count(*) - count(v) from (
          select t.user_id, v from
            (select user_id, number, decoded_value as v from #{@prefix}emulated) as t
            join #{@prefix}joined
            on t.user_id = #{@prefix}joined.user_id and t.number >= 0 and t.number < 10
            where number = 3
        ) x
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "join between emulated subquery and subquery with aggregation", do:
      assert_query """
        select count(v), avg(rows) from
        (select user_id, decoded_value as v from #{@prefix}emulated) as t1 inner join
        (select user_id as uid, count(*) as rows from #{@prefix}joined group by user_id) as t2 on user_id = uid
      """, %{rows: [%{occurrences: 1, row: [10, 1.0]}]}

    test "join between emulated table and subquery with subquery", do:
      assert_query """
        select count(v), avg(age) from
        (select user_id, decoded_value as v from #{@prefix}emulated) as t1 inner join
        (select user_id as uid, age + 1 as age from
          (select user_id, age * 2 as age from #{@prefix}joined) as t
        ) as t2 on user_id = uid
      """, %{rows: [%{occurrences: 1, row: [10, 61.0]}]}

    test "left join with filter in subquery" do
      assert_query """
        select value from (
          select #{@prefix}emulated.user_id, decoded_value as value from
            #{@prefix}emulated left join (select user_id as uid from #{@prefix}joined) as t on user_id = uid
            where t.uid is null) as t
      """, %{rows: [%{occurrences: 10, row: ["a b c"]}]}
    end

    test "left join with filter in top query" do
      assert_query """
        select decoded_value from #{@prefix}emulated left join
          (select user_id as uid from #{@prefix}joined) as t on user_id = uid and decoded_value = 'a b c'
        where t.uid is null
      """, %{rows: [%{occurrences: 10, row: ["a b c"]}]}
    end
  end

  test "#{@prefix}emulated subqueries with different case columns" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}joined", ["age"], [30])

    assert_query "select Value from (select user_id, decoded_value as value from #{@prefix}emulated) as t",
      %{rows: [%{occurrences: 10, row: ["abc"]}]}

    assert_query """
      select Value from (select user_id, length(decoded_value) as vAlue from #{@prefix}emulated) as t where vALUE = 3
    """, %{rows: [%{occurrences: 10, row: [3]}]}

    assert_query """
      select aGe, vaLue from #{@prefix}joined inner join
      (select user_Id as Uid, decoded_value as Value from #{@prefix}emulated group by User_id, DeCoded_Value) as t
      on uSer_Id = uId
    """, %{rows: [%{occurrences: 10, row: [30, "abc"]}]}
  end

  test "[BUG] join with alias and decoded column" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["number"], [10])
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}joined", ["age"], [10])

    assert_query """
      select count(*) - count(v) from #{@prefix}joined as t1 inner join
        (select user_id, decoded_value as v, number as n from #{@prefix}emulated) as t2
        on t1.user_id = t2.user_id and t1.age = t2.n
    """, %{rows: [%{row: [10]}]}
  end

  test "[BUG]: multiple JOINs with complex filter on encoded columns" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated_real", ["value"], [Base.encode64("10")])
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}joined", ["age"], [10])

    assert_query """
      select count(*) from (
        select t1.user_id from #{@prefix}joined as t1 join #{@prefix}emulated as t2
        on t1.user_id = t2.user_id where decoded_value in ('10', '20')
      ) as t1 join #{@prefix}joined as t2 on t1.user_id = t2.user_id
    """, %{rows: [%{row: [10]}]}
  end
end
