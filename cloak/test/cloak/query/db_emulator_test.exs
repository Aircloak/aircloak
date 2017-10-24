defmodule Cloak.Query.DBEmulatorTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  @prefix "db_emulator_"

  setup_all do
    decoders = [
      %{method: "base64", columns: ["value"]},
      %{method: "text_to_date", columns: ["date"]},
      %{method: "text_to_integer", columns: ["number"]},
    ]
    :ok = Cloak.Test.DB.create_table("#{@prefix}emulated", "value TEXT, date TEXT, number TEXT",
      [decoders: decoders])
    :ok = Cloak.Test.DB.create_table("#{@prefix}joined", "age INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("#{@prefix}emulated")
    Cloak.Test.DB.clear_table("#{@prefix}joined")
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
    assert_query """
      select count(*) from (select user_id, value from #{@prefix}emulated order by value limit 10) as t
    """, %{rows: [%{occurrences: 1, row: [10]}]}
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
    assert_query """
      select value from (select user_id, value from #{@prefix}emulated where length(value) = 3) as t
    """, %{rows: [%{occurrences: 10, row: ["abc"]}]}
  end

  test "like on an emulated column" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["value"], [Base.encode64("aba")])
    :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated", ["value"], [Base.encode64("aca")])
    :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated", ["value"], [Base.encode64("bbb")])

    assert_query "select count(*) from #{@prefix}emulated where value like 'a_a%'",
      %{rows: [%{row: [20]}]}
  end

  describe "aggregation in emulated subqueries" do
    def aggregation_setup(_) do
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("abc"), "2016-11-02"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("x"), "2015-01-30"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("xyz"), "2014-02-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("abcde"), "2013-02-08"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("1234"), "2013-12-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [nil, nil])
    end

    setup [:aggregation_setup]

    test "count(*)" do
      assert_query "select avg(v) from (select user_id, count(*) as v from #{@prefix}emulated group by user_id) as t",
        %{rows: [%{occurrences: 1, row: [6.0]}]}
    end

    test "count(<column>)" do
      assert_query """
        select avg(v) from (select user_id, count(value) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [5.0]}]}
    end

    test "group by function" do
      assert_query """
        select v from
          (select user_id, left(value, 1) as v from #{@prefix}emulated group by user_id, left(value, 1)) as t
        order by v
      """, %{rows: [%{occurrences: 20, row: ["1"]}, %{occurrences: 20, row: ["a"]},
        %{occurrences: 20, row: ["x"]}, %{occurrences: 20, row: [nil]}]}
    end

    test "where function" do
      assert_query """
          select avg(v) from (select user_id, count(value) as v from
          #{@prefix}emulated where length(value) = 3 group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [2.0]}]}
    end

    test "having inequality" do
      :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["number"], ["3"])
      :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated", ["number"], ["7"])
      :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated", ["number"], ["30"])

      assert_query """
        select v from
          (select user_id, number + 2 as v from #{@prefix}emulated
          group by user_id, number having number >= 0 and number < 10) as t
        order by v
      """, %{rows: [%{occurrences: 10, row: [5]}, %{occurrences: 10, row: [9]}]}
    end

    test "nested having inequality" do
      :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["number"], ["3"])
      :ok = insert_rows(_user_ids = 11..20, "#{@prefix}emulated", ["number"], ["7"])
      :ok = insert_rows(_user_ids = 21..30, "#{@prefix}emulated", ["number"], ["30"])

      assert_query """
        select v from
          (select user_id, v from
            (select user_id, number + 2 as v from #{@prefix}emulated
            group by user_id, number having number >= 0 and number < 10) as t
          group by user_id, v) as foo
        order by v
      """, %{rows: [%{occurrences: 10, row: [5]}, %{occurrences: 10, row: [9]}]}
    end

    test "where inequality" do
      assert_query """
        select length(v) as l from
          (select user_id, left(value, 1) as v from #{@prefix}emulated
          where date >= '2015-01-01' and date < '2016-01-01'
          group by user_id, value) as t
        order by l
      """, %{rows: [%{occurrences: 20, row: [1]}]}
    end

    test "nested where inequality" do
      assert_query """
        select length(v) as l from
          (select user_id, v from
            (select user_id, left(value, 1) as v from #{@prefix}emulated
            where date >= '2015-01-01' and date < '2016-01-01'
            group by user_id, value) as t
          group by user_id, v) as foo
        order by l
      """, %{rows: [%{occurrences: 20, row: [1]}]}
    end

    test "having equality" do
      assert_query """
        select avg(v) from
          (select user_id, sum(length(value)) as v from #{@prefix}emulated
          group by user_id having max(length(value)) = 5) as t
      """, %{rows: [%{occurrences: 1, row: [16.0]}]}
    end

    test "avg" do
      assert_query """
        select round(avg(v)) from
          (select user_id, avg(length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3]}]}
    end

    test "stddev" do
      :ok = insert_rows(_user_ids = 21..21, "#{@prefix}emulated", ["value"], [Base.encode64("wasabi")])
      assert_query """
        select round(avg(v)) from
          (select user_id, stddev(length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [1]}]}
    end

    test "min/max/median with numbers" do
      assert_query """
        select * from (select user_id, min(length(value)), max(length(value)), median(length(value))
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, 1, 5, 3]}]}
    end

    test "min/max/median with text" do
      assert_query """
        select * from (select user_id, min(value), max(value), median(value)
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "1234", "xyz", "abcde"]}]}
    end

    test "min/max/median with date" do
      assert_query """
        select * from (select user_id, min(date), max(date), median(date)
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "2013-02-08", "2016-11-02", "2014-02-04"]}]}
    end

    test "selecting a grouped column under an alias" do
      assert_query """
        select * from (
          select user_id, value, value as alias from #{@prefix}emulated where value = 'x' group by user_id, value
        ) foo
      """, %{rows: [%{occurrences: 20, row: [:*, "x", "x"]}]}
    end
  end

  describe "distinct in emulated subqueries" do
    defp distinct_setup(_) do
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("abc"), "2016-11-02"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("x"), "2015-01-30"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("xyz"), "2014-02-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("abcde"), "2013-02-08"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [Base.encode64("1234"), "2013-12-04"])
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated",
        ["value", "date"], [nil, nil])
    end

    setup [:distinct_setup]

    test "count(distinct value)", do:
      assert_query """
        select avg(v) from (select user_id, count(distinct value) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [5.0]}]}

    test "count(distinct left(value))", do:
      assert_query """
        select avg(v) from
          (select user_id, count(distinct left(value, 1)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3.0]}]}

    test "select distinct", do:
      assert_query "select v from (select distinct user_id, length(value) as v from #{@prefix}emulated) as t",
        %{rows: [
          %{occurrences: 20, row: [1]}, %{occurrences: 20, row: [3]}, %{occurrences: 20, row: [4]},
          %{occurrences: 20, row: [5]}, %{occurrences: 20, row: [nil]}
        ]}

    test "avg(distinct)", do:
      assert_query """
        select avg(v) from
          (select user_id, avg(distinct length(value)) as v from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 1, row: [3.25]}]}

    test "distinct min/max/median with text" do
      assert_query """
        select * from (select user_id, min(distinct value), max(distinct value), median(distinct value)
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "1234", "xyz", "abcde"]}]}
    end

    test "distinct min/max/median with date" do
      assert_query """
        select * from (select user_id, min(distinct date), max(distinct date), median(distinct date)
          from #{@prefix}emulated group by user_id) as t
      """, %{rows: [%{occurrences: 20, row: [:*, "2013-02-08", "2016-11-02", "2014-02-04"]}]}
    end
  end

  describe "emulated joins" do
    defp join_setup(_) do
      :ok = insert_rows(_user_ids = 1..20, "#{@prefix}emulated", ["value"], [Base.encode64("a b c")])
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
        select count(value) from
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
        select extract_words(value) from
          #{@prefix}joined inner join (select user_id as uid, value from #{@prefix}emulated) as t on user_id = uid
      """, %{rows: rows}

      assert length(rows) == 3
      Enum.zip(["a", "b", "c"], rows)
      |> Enum.each(fn({value, row}) -> assert %{occurrences: 10, row: [^value]} = row end)
    end

    test "where inequality" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated", ["number"], ["3"])

      assert_query """
        select count(*) from
          (select user_id from #{@prefix}emulated where number >= 0 and number < 10 group by user_id) x
          inner join
          (select user_id from #{@prefix}joined group by user_id) y
          on x.user_id = y.user_id
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "nested where inequality" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated", ["number"], ["3"])

      assert_query """
        select count(*) from
          (select user_id from
            (select user_id from #{@prefix}emulated where number >= 0 and number < 10 group by user_id) z
          group by user_id) x
          inner join
          (select user_id from #{@prefix}joined group by user_id) y
          on x.user_id = y.user_id
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "range in a join condition" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated", ["number"], ["3"])

      assert_query """
        select count(*) from
          #{@prefix}emulated
          join #{@prefix}joined
          on #{@prefix}emulated.user_id = #{@prefix}joined.user_id
          and #{@prefix}emulated.number >= 0 and #{@prefix}emulated.number < 10
          where number = 3
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "range in a nested join condition" do
      :ok = insert_rows(_user_ids = 21..25, "#{@prefix}emulated", ["number"], ["3"])

      assert_query """
        select count(*) from (
          select #{@prefix}emulated.user_id from
            #{@prefix}emulated
            join #{@prefix}joined
            on #{@prefix}emulated.user_id = #{@prefix}joined.user_id
            and #{@prefix}emulated.number >= 0 and #{@prefix}emulated.number < 10
            where number = 3
        ) x
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "join between emulated subquery and subquery with aggregation", do:
      assert_query """
        select count(*), avg(rows) from
        (select user_id, value from #{@prefix}emulated) as t1 inner join
        (select user_id as uid, count(*) as rows from #{@prefix}joined group by user_id) as t2 on user_id = uid
      """, %{rows: [%{occurrences: 1, row: [10, 1.0]}]}

    test "join between emulated table and subquery with subquery", do:
      assert_query """
        select count(*), avg(age) from
        (select user_id, value from #{@prefix}emulated) as t1 inner join
        (select user_id as uid, age + 1 as age from
          (select user_id, age * 2 as age from #{@prefix}joined) as t
        ) as t2 on user_id = uid
      """, %{rows: [%{occurrences: 1, row: [10, 61.0]}]}

      test "left join with filter in subquery" do
        assert_query """
          select value from (
            select #{@prefix}emulated.user_id, value from
              #{@prefix}emulated left join (select user_id as uid from #{@prefix}joined) as t on user_id = uid
              where t.uid is null) as t
        """, %{rows: [%{occurrences: 10, row: ["a b c"]}]}
      end

      test "left join with filter in top query" do
        assert_query """
          select value from #{@prefix}emulated left join
            (select user_id as uid from #{@prefix}joined) as t on user_id = uid and value = 'a b c'
          where t.uid is null
        """, %{rows: [%{occurrences: 10, row: ["a b c"]}]}
      end
  end

  test "emulated subqueries with extra dummy columns" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}joined", ["age"], [30])

    assert_query "select value from (select user_id, '', length(value), value from #{@prefix}emulated) as t",
      %{rows: [%{occurrences: 10, row: ["abc"]}]}

    assert_query """
      select value from (select '', user_id, left(value, 1) as value from #{@prefix}emulated) as t where value = 'a'
    """, %{rows: [%{occurrences: 10, row: ["a"]}]}

    assert_query """
      select age, value from #{@prefix}joined inner join
      (select '', user_id as uid, value from #{@prefix}emulated group by user_id, value) as t on user_id = uid
    """, %{rows: [%{occurrences: 10, row: [30, "abc"]}]}
  end

  test "#{@prefix}emulated subqueries with different case columns" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}joined", ["age"], [30])

    assert_query "select Value from (select user_id, length(vaLue), vaLue from #{@prefix}emulated) as t",
      %{rows: [%{occurrences: 10, row: ["abc"]}]}

    assert_query """
      select Value from (select user_id, left(vaLue, 1) as vAlue from #{@prefix}emulated) as t where vALUE = 'a'
    """, %{rows: [%{occurrences: 10, row: ["a"]}]}

    assert_query """
      select aGe, vaLue from #{@prefix}joined inner join
      (select user_Id as Uid, Value from #{@prefix}emulated group by User_id, VaLue) as t on uSer_Id = uId
    """, %{rows: [%{occurrences: 10, row: [30, "abc"]}]}
  end

  test "[BUG] join with alias and decoded column" do
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}emulated", ["number"], ["10"])
    :ok = insert_rows(_user_ids = 1..10, "#{@prefix}joined", ["age"], [10])

    assert_query """
      select count(*) from #{@prefix}joined as t1 inner join #{@prefix}emulated as t2
        on t1.user_id = t2.user_id and t1.age = t2.number
    """, %{rows: [%{row: [10]}]}
  end
end
