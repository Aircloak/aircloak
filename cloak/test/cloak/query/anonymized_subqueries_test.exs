defmodule Cloak.Query.AnonymizedSubqueriesTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("anon_sq", "i INTEGER, i2 INTEGER")
    for i <- 1..10, do: :ok = insert_rows(_user_ids = 1..15, "anon_sq", ["i"], [i])
    for i <- 1..5, do: :ok = insert_rows(_user_ids = i..i, "anon_sq", ["i2"], [i])

    for data_source <- Cloak.DataSource.all() do
      Cloak.TestShadowCache.safe(data_source, "anon_sq", "i", Enum.into(1..10, []))
    end

    :ok
  end

  test "count all" do
    assert_query("select count(*) from (select i from anon_sq group by i) as t", %{
      rows: [%{row: [11]}]
    })
  end

  test "aggregator over censored data" do
    assert_query("select sum(i2) from (select distinct i2 from anon_sq) as t", %{
      rows: [%{row: [nil]}]
    })
  end

  test "sums with where" do
    assert_query("select sum(i), sum(c) from (select i, count(*) as c from anon_sq group by i) as t where i <= 5", %{
      rows: [%{row: [15, 75]}]
    })
  end

  test "limit, offset and order by" do
    assert_query(
      """
        select i from (select i from anon_sq group by 1 order by 1 desc limit 5) as t order by 1 asc offset 2
      """,
      %{rows: [%{row: [9]}, %{row: [10]}, %{row: [nil]}]}
    )
  end

  test "chained where / having" do
    assert_query(
      """
        select i from (
          select i from (
            select i from anon_sq group by i having i <> 5
          ) as t where i in (1, 3, 5, 6)
        ) as t where i not in (1, 6)
      """,
      %{
        rows: [%{row: [3]}]
      }
    )
  end

  test "multiple anonymized subqueries" do
    assert_query(
      """
        select count(*) from
          (select i from anon_sq group by i having i <> 3) as t1
          join
          (select i from anon_sq group by i having i > 7) as t2
          on t1.i <> t2.i
      """,
      %{
        rows: [%{row: [24]}]
      }
    )
  end

  test "distinct in subquery is anonymized" do
    assert_query("select count(*) from (select distinct i from anon_sq) as t", %{
      rows: [%{row: [11]}]
    })
  end

  test "propagate unreliability flag through simple select" do
    assert_query(
      """
        select m + 1 from (
          select median(i) as m from anon_sq group by i % 1 order by m
        ) as t order by 1 desc limit 2
      """,
      %{
        rows: [
          %{row: [nil], unreliable: true},
          %{row: [7], unreliable: false}
        ]
      }
    )
  end

  test "propagate unreliability flag through aggregation" do
    assert_query(
      "select sum(m) from (select median(i) as m from anon_sq group by i % 1) as t",
      %{rows: [%{row: [6], unreliable: true}]}
    )
  end

  test "propagate unreliability flag through joins" do
    assert_query(
      """
        select * from (
          select median(i) as m1 from anon_sq group by i % 1
        ) as t1 cross join (
          select median(i) as m2 from anon_sq group by i2 % 1
        ) as t2
        order by 1, 2
      """,
      %{
        rows: [
          %{row: [6, 5], unreliable: false},
          %{row: [6, nil], unreliable: true},
          %{row: [nil, 5], unreliable: true},
          %{row: [nil, nil], unreliable: true}
        ]
      }
    )
  end
end
