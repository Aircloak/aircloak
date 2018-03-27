defmodule Cloak.Query.DBEmulatorTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers, except: [assert_query: 3]

  @prefix "db_emulator_"
  @emulated "cloak_test.#{@prefix}emulated"
  @emulated_insert "#{@prefix}emulated"
  @joined "#{@prefix}joined"
  @vt "#{@prefix}temp"

  defmacrop assert_query(query, vt_query, expected_response) do
    quote do
      :ok = Cloak.Test.DB.create_table(@vt, nil, skip_db_create: true, query: unquote(vt_query))
      on_exit(fn -> Cloak.Test.DB.delete_table(@vt) end)
      assert unquote(expected_response) = assert_query_consistency(unquote(query))
    end
  end

  setup_all do
    :ok = Cloak.Test.DB.create_table(@emulated_insert, "value TEXT, date TEXT, number INTEGER")
    :ok = Cloak.Test.DB.create_table(@joined, "age INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table(@emulated_insert)
    Cloak.Test.DB.clear_table(@joined)
    :ok
  end

  test "simple emulated subqueries" do
    :ok = insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["value"], [Base.encode64("aaa")])

    :ok =
      insert_rows(_user_ids = 11..20, "#{@emulated_insert}", ["value"], [Base.encode64("bbb")])

    :ok = insert_rows(_user_ids = 21..30, "#{@emulated_insert}", ["value"], [nil])

    assert_query(
      "select count(v) from #{@vt}",
      """
        select user_id, dec_b64(value) AS v from #{@emulated} where v = 'aaa'
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )

    assert_query(
      "select count(v) from #{@vt}",
      """
        select user_id, dec_b64(value) AS v from #{@emulated} where v is not null
      """,
      %{rows: [%{occurrences: 1, row: [20]}]}
    )

    assert_query(
      "select count(*) from #{@vt}",
      """
        select user_id from #{@emulated} order by dec_b64(value) limit 10
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )

    assert_query(
      "select count(*) from #{@vt}",
      """
        select user_id from #{@emulated} order by dec_b64(value) limit 10 offset 10
      """,
      %{rows: [%{occurrences: 1, row: [10]}]}
    )
  end

  test "emulated subqueries with functions" do
    :ok = insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["value"], [Base.encode64("abc")])
    :ok = insert_rows(_user_ids = 11..20, "#{@emulated_insert}", ["value"], [Base.encode64("x")])

    assert_query(
      "select l from #{@vt} order by l desc",
      """
        select user_id, length(dec_b64(value)) as l from #{@emulated}
      """,
      %{rows: [%{occurrences: 10, row: [3]}, %{occurrences: 10, row: [1]}]}
    )

    assert_query(
      "select v from #{@vt} where length(v) = 1",
      """
        select user_id, dec_b64(value) as v from #{@emulated}
      """,
      %{rows: [%{occurrences: 10, row: ["x"]}]}
    )

    assert_query(
      "select v from #{@vt}",
      """
        select user_id, dec_b64(value) AS v from #{@emulated} where length(v) = 3
      """,
      %{rows: [%{occurrences: 10, row: ["abc"]}]}
    )
  end

  test "like on an emulated column" do
    :ok = insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["value"], [Base.encode64("aba")])

    :ok =
      insert_rows(_user_ids = 11..20, "#{@emulated_insert}", ["value"], [Base.encode64("aca")])

    :ok =
      insert_rows(_user_ids = 21..30, "#{@emulated_insert}", ["value"], [Base.encode64("bbb")])

    assert_query(
      "select count(*) from #{@vt}",
      "select * from #{@emulated} where dec_b64(value) like 'a_a%'",
      %{rows: [%{row: [20]}]}
    )
  end

  describe "aggregation in emulated subqueries" do
    setup do
      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("abc"),
          "2016-11-02"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("x"),
          "2015-01-30"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("xyz"),
          "2014-02-04"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("abcde"),
          "2013-02-08"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("1234"),
          "2013-12-04"
        ])

      :ok = insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [nil, nil])
    end

    test "count(*)" do
      assert_query(
        "select avg(v) from #{@vt}",
        "select user_id, count(*) as v from #{@emulated} group by user_id",
        %{rows: [%{occurrences: 1, row: [6.0]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "count(<column>)" do
      assert_query(
        "select avg(v) from #{@vt}",
        """
          select user_id, count(dec_b64(value)) as v from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 1, row: [5.0]}]}
      )
    end

    test "group by function" do
      assert_query(
        "select v from #{@vt} order by v",
        """
          select user_id, dec_b64(value) as v from #{@emulated} group by user_id, v
        """,
        %{
          rows: [
            %{occurrences: 20, row: ["1234"]},
            %{occurrences: 20, row: ["abc"]},
            %{occurrences: 20, row: ["abcde"]},
            %{occurrences: 20, row: ["x"]},
            %{occurrences: 20, row: ["xyz"]},
            %{occurrences: 20, row: [nil]}
          ]
        }
      )
    end

    @tag pending: "mysterious indexing error"
    test "where function" do
      assert_query(
        "select avg(v) from #{@vt}",
        """
          select user_id, count(dec_b64(value)) as v from
            #{@emulated} where length(dec_b64(value)) = 3 group by user_id
        """,
        %{rows: [%{occurrences: 1, row: [2.0]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "having inequality" do
      :ok = insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["number"], [3])
      :ok = insert_rows(_user_ids = 11..20, "#{@emulated_insert}", ["number"], [7])
      :ok = insert_rows(_user_ids = 21..30, "#{@emulated_insert}", ["number"], [30])

      assert_query(
        "select n + c from #{@vt} order by n + c",
        """
          select user_id, number + 2 as n, count(dec_b64(value)) as c from #{@emulated}
            group by user_id, number having number >= 0 and number < 10
        """,
        %{rows: [%{occurrences: 10, row: [5]}, %{occurrences: 10, row: [9]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "nested having inequality" do
      :ok = insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["number"], [3])
      :ok = insert_rows(_user_ids = 11..20, "#{@emulated_insert}", ["number"], [7])
      :ok = insert_rows(_user_ids = 21..30, "#{@emulated_insert}", ["number"], [30])

      assert_query(
        "select n + c from #{@vt} order by n",
        """
          select user_id, n, c from
            (select user_id, number + 2 as n, count(dec_b64(value)) as c from #{@emulated}
            group by user_id, number having number >= 0 and number < 10) as t
          group by user_id, n, c
        """,
        %{rows: [%{occurrences: 10, row: [5]}, %{occurrences: 10, row: [9]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "where inequality" do
      assert_query(
        "select sum(c) from #{@vt}",
        """
          select user_id, count(dec_b64(value)) as c from #{@emulated}
            where cast(date as date) >= '2015-01-01' and cast(date as date) < '2016-01-01'
            group by user_id
        """,
        %{rows: [%{occurrences: 1, row: [20]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "nested where inequality" do
      assert_query(
        "select sum(c) from #{@vt}",
        """
          select user_id, c from
            (select user_id, count(dec_b64(value)) as c from #{@emulated}
            where cast(date as date) >= '2015-01-01' and cast(date as date) < '2016-01-01'
            group by user_id) as t
          group by user_id, c
        """,
        %{rows: [%{occurrences: 1, row: [20]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "having equality" do
      assert_query(
        "select avg(v) from #{@vt}",
        """
          select user_id, sum(length(dec_b64(value))) as v from #{@emulated}
            group by user_id having max(length(dec_b64(value))) = 5
        """,
        %{rows: [%{occurrences: 1, row: [16.0]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "avg" do
      assert_query(
        "select round(avg(v)) from #{@vt}",
        """
          select user_id, avg(length(dec_b64(value))) as v from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 1, row: [3]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "stddev" do
      :ok =
        insert_rows(_user_ids = 21..21, "#{@emulated_insert}", ["value"], [
          Base.encode64("wasabi")
        ])

      assert_query(
        "select round(avg(v)) from #{@vt}",
        """
          select user_id, stddev(length(dec_b64(value))) as v from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 1, row: [1]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "min/max/median with numbers" do
      assert_query(
        "select * from #{@vt}",
        """
          select user_id, min(length(dec_b64(value))), max(length(dec_b64(value))), median(length(dec_b64(value)))
          from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 20, row: [:*, 1, 5, 3]}]}
      )
    end

    @tag pending: "min over text is incorrectly disallowed in virtual tables"
    test "min/max/median with text" do
      assert_query(
        "select * from #{@vt}",
        """
          select user_id, min(dec_b64(value)), max(dec_b64(value)), median(dec_b64(value))
            from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 20, row: [:*, "1234", "xyz", "abcde"]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "min/max/median with date" do
      assert_query(
        "select * from #{@vt}",
        """
          select user_id, min(cast(date as date)),
            max(cast(date as date)), median(cast(date as date)), count(dec_b64(value)) as c
          from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 20, row: [:*, "2013-02-08", "2016-11-02", "2014-02-04", 5]}]}
      )
    end

    test "selecting a grouped column under an alias" do
      assert_query(
        "select * from #{@vt}",
        """
          select user_id, dec_b64(value) as v, dec_b64(value) as alias
            from #{@emulated} where v = 'x' group by user_id, v
        """,
        %{rows: [%{occurrences: 20, row: [:*, "x", "x"]}]}
      )
    end
  end

  describe "distinct in emulated subqueries" do
    setup do
      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("abc"),
          "2016-11-02"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("x"),
          "2015-01-30"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("xyz"),
          "2014-02-04"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("abcde"),
          "2013-02-08"
        ])

      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [
          Base.encode64("1234"),
          "2013-12-04"
        ])

      :ok = insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value", "date"], [nil, nil])
    end

    @tag pending: "mysterious indexing error"
    test "count(distinct value)",
      do:
        assert_query(
          "select avg(v) from #{@vt}",
          """
            select user_id, count(distinct dec_b64(value)) as v
              from #{@emulated} group by user_id
          """,
          %{rows: [%{occurrences: 1, row: [5.0]}]}
        )

    @tag pending: "mysterious indexing error"
    test "count(distinct length(value))",
      do:
        assert_query(
          "select avg(v) from #{@vt}",
          """
            select user_id, count(distinct length(dec_b64(value))) as v from #{@emulated} group by user_id
          """,
          %{rows: [%{occurrences: 1, row: [4.0]}]}
        )

    @tag pending: "select distinct doesn't seem to work in virtual table"
    test "select distinct",
      do:
        assert_query(
          "select v from #{@vt}",
          "select distinct user_id, length(dec_b64(value)) as v from #{@emulated}",
          %{
            rows: [
              %{occurrences: 20, row: [1]},
              %{occurrences: 20, row: [3]},
              %{occurrences: 20, row: [4]},
              %{occurrences: 20, row: [5]},
              %{occurrences: 20, row: [nil]}
            ]
          }
        )

    @tag pending: "mysterious indexing error"
    test "avg(distinct)",
      do:
        assert_query(
          "select avg(v) from  #{@vt}",
          """
            select user_id, avg(distinct length(dec_b64(value))) as v from #{@emulated} group by user_id
          """,
          %{rows: [%{occurrences: 1, row: [3.25]}]}
        )

    @tag pending: "min over text is incorrectly disallowed in virtual tables"
    test "distinct min/max/median with text" do
      assert_query(
        "select * from #{@vt}",
        """
          select user_id,
            min(distinct dec_b64(value)), max(distinct dec_b64(value)), median(distinct dec_b64(value))
            from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 20, row: [:*, "1234", "xyz", "abcde"]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "distinct min/max/median with date" do
      assert_query(
        "select * from #{@vt}",
        """
          select user_id, min(distinct cast(date as date)),
            max(distinct cast(date as date)), median(distinct cast(date as date)), count(dec_b64(value)) as c
            from #{@emulated} group by user_id
        """,
        %{rows: [%{occurrences: 20, row: [:*, "2013-02-08", "2016-11-02", "2014-02-04", 5]}]}
      )
    end
  end

  describe "emulated joins" do
    setup do
      :ok =
        insert_rows(_user_ids = 1..20, "#{@emulated_insert}", ["value"], [Base.encode64("a b c")])

      :ok = insert_rows(_user_ids = 11..25, "#{@joined}", ["age"], [30])
    end

    test "cross join",
      do:
        assert_query(
          "select count(age) from #{@vt}, #{@joined} where #{@vt}.user_id = #{@joined}.user_id",
          "select * from #{@emulated}",
          %{rows: [%{occurrences: 1, row: [10]}]}
        )

    test "inner join",
      do:
        assert_query(
          """
            select count(v) from
              #{@vt} inner join (select user_id as uid from #{@joined}) as t on user_id = uid
          """,
          "select user_id, dec_b64(value) as v from #{@emulated}",
          %{rows: [%{occurrences: 1, row: [10]}]}
        )

    test "left join between table and subquery" do
      assert_query(
        """
          select count(v) from
            #{@vt} left join (select user_id as uid from #{@joined}) as t on user_id = uid
        """,
        "select user_id, dec_b64(value) as v from #{@emulated}",
        %{rows: [%{occurrences: 1, row: [20]}]}
      )

      assert_query(
        """
          select count(age) from
            #{@vt} left join #{@joined}
            on #{@vt}.user_id = #{@joined}.user_id where age = 30
        """,
        "select user_id, dec_b64(value) as v from #{@emulated}",
        %{rows: [%{occurrences: 1, row: [10]}]}
      )
    end

    @tag pending: "selecting count(*) produces 15, while selecting v produces a total of 10 rows"
    test "right join",
      do:
        assert_query(
          """
          select v from
            #{@vt} right join #{@joined}
            on #{@vt}.user_id = #{@joined}.user_id
          """,
          "select user_id, dec_b64(value) as v from #{@emulated}",
          %{rows: [%{occurrences: 10, row: ["a b c"]}, %{occurrences: 5, row: [nil]}]}
        )

    @tag pending: "crash in Validation.verify_all_uid_columns_are_compared_in_joins"
    test "join with a row splitter function" do
      assert_query(
        "select extract_words(value) from #{@joined} inner join #{@vt} on user_id = uid",
        "select user_id as uid, dec_b64(value) as value from #{@emulated}",
        %{rows: rows}
      )

      assert length(rows) == 3

      Enum.zip(["a", "b", "c"], rows)
      |> Enum.each(fn {value, row} -> assert %{occurrences: 10, row: [^value]} = row end)
    end

    test "where inequality" do
      :ok = insert_rows(_user_ids = 21..25, "#{@emulated_insert}", ["number"], [3])

      assert_query(
        """
        select count(*) - count(x.v) from
          #{@vt} as x
          inner join
          (select user_id from #{@joined} group by user_id) y
          on x.user_id = y.user_id
        """,
        "select user_id, dec_b64(value) as v from #{@emulated} where number >= 0 and number < 10 group by user_id, v",
        %{rows: [%{occurrences: 1, row: [5]}]}
      )
    end

    @tag pending: "mysterious indexing error"
    test "nested where inequality" do
      :ok = insert_rows(_user_ids = 21..25, "#{@emulated_insert}", ["number"], [3])

      assert_query(
        """
        select count(c) from
          (select user_id, c from #{@vt}
          group by user_id, c) x
          inner join
          (select user_id from #{@joined} group by user_id) y
          on x.user_id = y.user_id
        """,
        """
          select user_id, count(dec_b64(value)) as c from #{@emulated}
          where number >= 0 and number < 10 group by user_id
        """,
        %{rows: [%{occurrences: 1, row: [5]}]}
      )
    end

    test "range in a join condition" do
      :ok = insert_rows(_user_ids = 21..25, "#{@emulated_insert}", ["number"], [3])

      assert_query(
        """
        select count(*) - count(v) from
          #{@vt} as t join #{@joined}
          on t.user_id = #{@joined}.user_id and t.number >= 0 and t.number < 10
          where number = 3
        """,
        "select user_id, number, dec_b64(value) as v from #{@emulated}",
        %{rows: [%{occurrences: 1, row: [5]}]}
      )
    end

    test "range in a nested join condition" do
      :ok = insert_rows(_user_ids = 21..25, "#{@emulated_insert}", ["number"], [3])

      assert_query(
        """
        select count(*) - count(v) from (
          select t.user_id, v from
            #{@vt} as t join #{@joined}
            on t.user_id = #{@joined}.user_id and t.number >= 0 and t.number < 10
            where number = 3
        ) x
        """,
        "select user_id, number, dec_b64(value) as v from #{@emulated}",
        %{rows: [%{occurrences: 1, row: [5]}]}
      )
    end

    test "join between emulated subquery and subquery with aggregation",
      do:
        assert_query(
          """
          select count(v), avg(rows) from
          #{@vt} as t1 inner join
          (select user_id as uid, count(*) as rows from #{@joined} group by user_id) as t2 on user_id = uid
          """,
          "select user_id, dec_b64(value) as v from #{@emulated}",
          %{rows: [%{occurrences: 1, row: [10, 1.0]}]}
        )

    test "join between emulated table and subquery with subquery",
      do:
        assert_query(
          """
          select count(v), avg(age) from
          #{@vt} as t1 inner join
          (select user_id as uid, age + 1 as age from
            (select user_id, age * 2 as age from #{@joined}) as t
          ) as t2 on user_id = uid
          """,
          "select user_id, dec_b64(value) as v from #{@emulated}",
          %{rows: [%{occurrences: 1, row: [10, 61.0]}]}
        )

    test "left join with filter in subquery" do
      assert_query(
        """
        select value from (
          select #{@vt}.user_id, value from
            #{@vt} left join (select user_id as uid from #{@joined}) as t on user_id = uid
            where t.uid is null) as t
        """,
        "select user_id, dec_b64(value) as value from #{@emulated}",
        %{rows: [%{occurrences: 10, row: ["a b c"]}]}
      )
    end

    test "left join with filter in top query" do
      assert_query(
        """
        select value from #{@vt} left join
          (select user_id as uid from #{@joined}) as t on user_id = uid and value = 'a b c'
        where t.uid is null
        """,
        "select user_id, dec_b64(value) as value from #{@emulated}",
        %{rows: [%{occurrences: 10, row: ["a b c"]}]}
      )
    end
  end

  describe "#{@emulated} subqueries with different case columns" do
    setup do
      :ok =
        insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["value"], [Base.encode64("abc")])

      :ok = insert_rows(_user_ids = 1..10, "#{@joined}", ["age"], [30])
    end

    test "simple subquery" do
      assert_query(
        "select Value from #{@vt}",
        "select user_id, dec_b64(value) as value from #{@emulated}",
        %{rows: [%{occurrences: 10, row: ["abc"]}]}
      )
    end

    test "functions and conditions" do
      assert_query(
        "select Value from #{@vt} where vALUE = 3",
        "select user_id, length(dec_b64(value)) as vAlue from #{@emulated}",
        %{rows: [%{occurrences: 10, row: [3]}]}
      )
    end

    @tag pending: "crash in Validation.verify_all_uid_columns_are_compared_in_joins"
    test "join" do
      assert_query(
        "select aGe, vaLue from #{@joined} inner join #{@vt} on uSer_Id = uId",
        "select user_Id as Uid, dec_b64(value) as Value from #{@emulated} group by User_id, vAlUe",
        %{rows: [%{occurrences: 10, row: [30, "abc"]}]}
      )
    end
  end

  test "[BUG] join with alias and decoded column" do
    :ok = insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["number"], [10])
    :ok = insert_rows(_user_ids = 1..10, "#{@joined}", ["age"], [10])

    assert_query(
      """
      select count(*) - count(v) from #{@joined} as t1 inner join #{@vt} as t2
        on t1.user_id = t2.user_id and t1.age = t2.n
      """,
      "select user_id, dec_b64(value) as v, number as n from #{@emulated}",
      %{rows: [%{row: [10]}]}
    )
  end

  test "[BUG]: multiple JOINs with complex filter on encoded columns" do
    :ok = insert_rows(_user_ids = 1..10, "#{@emulated_insert}", ["value"], [Base.encode64("10")])
    :ok = insert_rows(_user_ids = 1..10, "#{@joined}", ["age"], [10])

    assert_query(
      """
      select count(*) from (
        select t1.user_id from #{@joined} as t1 join #{@vt} as t2
        on t1.user_id = t2.user_id where value in ('10', '20')
      ) as t1 join #{@joined} as t2 on t1.user_id = t2.user_id
      """,
      "select user_id, dec_b64(value) as value from #{@emulated}",
      %{rows: [%{row: [10]}]}
    )
  end
end
