defmodule Cloak.Query.EmulatedAndProjectedTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  @prefix "emulated_and_projected_"

  setup_all do
    :ok = Cloak.Test.DB.create_table("#{@prefix}main", "dummy BOOLEAN")
    :ok = Cloak.Test.DB.create_table(
      "#{@prefix}emulated", "id TEXT, user_id_fk TEXT, value TEXT, value2 TEXT, num INTEGER",
      add_user_id: false,
      decoders: [
        %{method: "base64", columns: ["value"]},
        %{method: "text_to_integer", columns: ["id"]},
      ],
      projection: %{
        table: "#{@prefix}main",
        foreign_key: "user_id_fk",
        primary_key: "user_id",
      },
    )
    :ok = Cloak.Test.DB.create_table(
      "#{@prefix}emulated2", "foreign_key INTEGER, value TEXT",
      add_user_id: false,
      decoders: [%{method: "base64", columns: ["value"]}],
      projection: %{
        table: "#{@prefix}emulated",
        foreign_key: "foreign_key",
        primary_key: "id",
      },
    )
    :ok = Cloak.Test.DB.create_table("#{@prefix}joined", "age INTEGER")
    :ok
  end

  setup do
    Cloak.Test.DB.clear_table("#{@prefix}emulated2")
    Cloak.Test.DB.clear_table("#{@prefix}emulated")
    Cloak.Test.DB.clear_table("#{@prefix}main")
    Cloak.Test.DB.clear_table("#{@prefix}joined")
    :ok = insert_rows(_user_ids = 1..100, "#{@prefix}main", [], [])
    :ok
  end

  describe "simple queries" do
    setup [:simple_setup]

    test "where", do:
      assert_query "select count(value) from #{@prefix}emulated where value = 'aaa'",
        %{rows: [%{occurrences: 1, row: [10]}]}

    test "non-nulls", do:
      assert_query "select count(value) from #{@prefix}emulated where value is not null",
        %{rows: [%{occurrences: 1, row: [20]}]}

    test "order by", do:
      assert_query "select length(value) as l from #{@prefix}emulated order by l desc",
        %{rows: [%{occurrences: 10, row: [nil]}, %{occurrences: 10, row: [3]}, %{occurrences: 10, row: [1]}]}

    test "non-selected order by", do:
      assert_query "select value from #{@prefix}emulated order by num",
        %{rows: [
          %{occurrences: 10, row: [nil]},
          %{occurrences: 10, row: ["b"]},
          %{occurrences: 10, row: ["aaa"]}
        ]}

    test "non-selected aggregate in order by", do:
      assert_query "select num from #{@prefix}emulated group by num order by num, count(*)",
        %{rows: [
          %{occurrences: 1, row: [1]},
          %{occurrences: 1, row: [2]},
          %{occurrences: 1, row: [3]}
        ]}

    test "aliased", do:
      assert_query "select count(e.value) from #{@prefix}emulated e where e.value = 'aaa'",
        %{rows: [%{occurrences: 1, row: [10]}]}
  end

  describe "simple emulated subqueries" do
    defp simple_subqueries_setup(_) do
      :ok = insert_emulated_row(_user_ids = 1..10, ["value", "num"], [Base.encode64("aaa"), 3])
      :ok = insert_emulated_row(_user_ids = 11..20, ["value", "num"], [Base.encode64("bbb"), 2])
      :ok = insert_emulated_row(_user_ids = 21..30, ["value", "num"], [nil, 1])
    end

    setup [:simple_subqueries_setup]

    test "where", do:
      assert_query "select count(value) from (select user_id, value from #{@prefix}emulated where value = 'aaa') as t",
        %{rows: [%{occurrences: 1, row: [10]}]}

    test "non-nulls", do:
      assert_query """
          select count(value) from (select user_id, value from #{@prefix}emulated where value is not null) as t
        """, %{rows: [%{occurrences: 1, row: [20]}]}

    test "limit", do:
      assert_query "select count(*) from (select user_id, value from #{@prefix}emulated order by value limit 10) as t",
        %{rows: [%{occurrences: 1, row: [10]}]}

    test "limit and offset", do:
      assert_query """
          select count(*) from (select user_id, value from #{@prefix}emulated order by value limit 10 offset 10) as t
        """, %{rows: [%{occurrences: 1, row: [10]}]}

    test "non-selected order by", do:
      assert_query "select value from (select user_id, value from #{@prefix}emulated order by num limit 1) as t",
        %{rows: [%{occurrences: 10, row: [nil]}]}

    test "non-selected aggregate order by", do:
      assert_query(
        "select num from " <>
          "(select user_id, num from #{@prefix}emulated group by 1, 2 order by num, count(*) limit 1) as t",
        %{rows: [%{occurrences: 10, row: [1]}]}
      )

    test "implicitly selected uid", do:
      assert_query "select count(value) from (select value from #{@prefix}emulated where value = 'aaa') as t",
        %{rows: [%{occurrences: 1, row: [10]}]}

    test "select all from a query with an implicitly selected uid", do:
      assert_query "select * from (select value from #{@prefix}emulated where value = 'aaa') as t",
        %{columns: ["value"], rows: [%{occurrences: 10, row: ["aaa"]}]}
  end

  describe "emulated subqueries with functions" do
    defp subqueries_with_functions_setup(_) do
      :ok = insert_emulated_row(_user_ids = 1..10, ["value"], [Base.encode64("abc")])
      :ok = insert_emulated_row(_user_ids = 11..20, ["value"], [Base.encode64("x")])
    end

    setup [:subqueries_with_functions_setup]

    test "length", do:
      assert_query "select l from (select user_id, length(value) as l from #{@prefix}emulated) as t order by l desc",
        %{rows: [%{occurrences: 10, row: [3]}, %{occurrences: 10, row: [1]}]}

    test "left", do:
      assert_query """
          select value from (select user_id, left(value, 1) as value from #{@prefix}emulated) as t where value = 'a'
        """, %{rows: [%{occurrences: 10, row: ["a"]}]}
  end

  describe "aggregation in emulated subqueries" do
    def aggregation_setup(_) do
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("abc")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("x")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("xyx")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("abcde")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("1234")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [nil])
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

    test "having inequality" do
      assert_query """
        select length(v) as l from
          (select user_id, left(value, 1) as v from #{@prefix}emulated
          group by user_id, value having length(value) >= 1 and length(value) < 2) as t
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

    test "min" do
      assert_query """
          select v from (select user_id, min(length(value)) as v from #{@prefix}emulated group by user_id) as t
        """, %{rows: [%{occurrences: 20, row: [1]}]}
    end

    test "max" do
      assert_query """
          select v from (select user_id, max(length(value)) as v from #{@prefix}emulated group by user_id) as t
        """, %{rows: [%{occurrences: 20, row: [5]}]}
    end

    test "avg" do
      assert_query """
          select round(avg(v)) from
          (select user_id, avg(length(value)) as v from #{@prefix}emulated group by user_id) as t
        """, %{rows: [%{occurrences: 1, row: [3]}]}
    end

    test "stddev" do
      assert_query """
          select round(avg(v)) from
          (select user_id, stddev(length(value)) as v from #{@prefix}emulated group by user_id) as t
        """, %{rows: [%{occurrences: 1, row: [1]}]}
    end

    test "median" do
      assert_query """
          select round(avg(v)) from
          (select user_id, median(length(value)) as v from #{@prefix}emulated group by user_id) as t
        """, %{rows: [%{occurrences: 1, row: [3]}]}
    end
  end

  describe "distinct in emulated subqueries" do
    defp distinct_setup(_) do
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("abc")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("x")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("xyx")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("abcde")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [Base.encode64("1234")])
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [nil])
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
  end

  describe "emulated joins" do
    defp join_setup(_) do
      :ok = insert_emulated_row(_user_ids = 1..20, ["value"], [nil])
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
  end

  describe "extract_matches" do
    setup [:simple_setup]

    test "extract_matches on a concatenated string" do
      assert_query(
        "SELECT extract_matches(value || value2, '\\w+') FROM #{@prefix}emulated GROUP BY 1",
        %{rows: [
          %{row: ["aaa"], occurrences: 1},
          %{row: ["b"], occurrences: 1},
          %{row: ["c"], occurrences: 1},
        ]}
      )
    end
  end

  test "double projected emulated query" do
    user_id_range = 1..100
    :ok = Cloak.Test.DB.insert_data("#{@prefix}emulated", ["user_id_fk", "id"],
      Enum.map(user_id_range, &["user#{&1}", "#{&1}"]))
    :ok = Cloak.Test.DB.insert_data("#{@prefix}emulated2", ["foreign_key", "value"],
      Enum.map(user_id_range, &[&1, Base.encode64("aaa")]))
    :ok = Cloak.Test.DB.insert_data("#{@prefix}emulated2", ["foreign_key", "value"],
      Enum.map(user_id_range, &[&1, Base.encode64("aaa")]))

    assert_query "select count(*) from #{@prefix}emulated2 where value = 'aaa'",
      %{rows: [%{occurrences: 1, row: [200]}]}
  end

  describe "low count checks" do
    test "low count check in a top-level emulated query" do
      :ok = insert_emulated_row(_user_ids = 1..5, ["value"], [Base.encode64("whatever")])
      :ok = insert_emulated_row(_user_ids = 6..6, ["value"], [Base.encode64("willy")])

      assert_query """
        select count(*) from
        #{@prefix}emulated
        where upper(value) ILIKE '%w%'
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "low count check in a join" do
      :ok = insert_emulated_row(_user_ids = 1..5, ["value"], [Base.encode64("whatever")])
      :ok = insert_emulated_row(_user_ids = 6..6, ["value"], [Base.encode64("willy")])
      :ok = insert_rows(_user_ids = 1..6, "#{@prefix}joined", [], [])

      assert_query """
        select count(*) from
        #{@prefix}emulated
        inner join #{@prefix}joined
        on #{@prefix}emulated.user_id = #{@prefix}joined.user_id
        where upper(value) ILIKE '%w%'
      """, %{rows: [%{occurrences: 1, row: [5]}]}
    end

    test "low count check in a join subquery"

    test "low count check in aggregated subquery"
  end

  defp simple_setup(_) do
    :ok = insert_emulated_row(_user_ids = 1..10, ["value", "num"], [Base.encode64("aaa"), 3])
    :ok = insert_emulated_row(_user_ids = 11..20, ["value", "num"], [Base.encode64("b"), 2])
    :ok = insert_emulated_row(_user_ids = 21..30, ["value", "num", "value2"], [nil, 1, "c"])
  end

  defp insert_emulated_row(user_id_range, columns, values), do:
    Cloak.Test.DB.insert_data("#{@prefix}emulated", ["user_id_fk" | columns],
      Enum.map(user_id_range, &["user#{&1}" | values]))
end
