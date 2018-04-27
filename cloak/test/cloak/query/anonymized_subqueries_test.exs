defmodule Cloak.Query.AnonymizedSubqueriesTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("anon_sq", "i INTEGER")
    for i <- 1..10, do: :ok = insert_rows(_user_ids = 1..10, "anon_sq", ["i"], [i])
    :ok
  end

  test "count all" do
    assert_query("select count(*) from (select i from anon_sq group by i) as t", %{
      rows: [%{row: [10]}]
    })
  end

  test "sums with where" do
    assert_query("select sum(i), sum(c) from (select i, count(*) as c from anon_sq group by i) as t where i <= 5", %{
      rows: [%{row: [15, 50]}]
    })
  end

  test "limit, offset and order by" do
    assert_query(
      """
        select i from (select i from anon_sq group by 1 order by 1 desc limit 5) as t order by 1 asc offset 2
      """,
      %{rows: [%{row: [8]}, %{row: [9]}, %{row: [10]}]}
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
end
