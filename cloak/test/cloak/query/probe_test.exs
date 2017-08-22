defmodule Cloak.Query.ProbeTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  alias Cloak.Sql.Expression
  alias Cloak.Query.Probe

  setup_all do
    :ok = Cloak.Test.DB.create_table("lcf_conditions", "x INTEGER, y INTEGER")
  end

  setup do
    Cloak.Test.DB.clear_table("lcf_conditions")
    :ok = insert_rows(_user_ids = 1..20, "lcf_conditions", ["x"], [0])
    :ok = insert_rows(_user_ids = 11..30, "lcf_conditions", ["x"], [1])
    :ok = insert_rows(_user_ids = 30..31, "lcf_conditions", ["x"], [2])
    :ok = insert_rows(_user_ids = 30..40, "lcf_conditions", ["x"], [nil])
  end

  defp make_query!(query_string) do
    [first_ds | rest_ds] = Cloak.DataSource.all()
    query = make_query!(query_string, first_ds)

    for data_source <- rest_ds, do:
      assert(Map.drop(query, [:features]) == Map.drop(make_query!(query_string, data_source), [:features]))

    query
  end

  defp make_query!(query_string, data_source) do
    query_string
    |> compile!(data_source)
    |> Probe.process()
    |> scrub_aliases()
    |> scrub_data_sources()
  end

  test "complex negative conditions in top query" do
    query = make_query!("select count(*) from lcf_conditions where round(x) <> 2 and round(x) <> 0")
    assert {:and, {:not, {:is, _, :null}}, {:comparison, _, :<>, %Expression{value: 0}}} = query.where
  end

  test "complex negative conditions in sub-query where" do
    query = make_query!("""
      select count(*) from (select user_id from lcf_conditions where round(x) <> 2 and round(x) <> 0) as t
    """)
    {:subquery, %{ast: subquery}} = query.from
    assert {:comparison, _, :<>, %Expression{value: 0}} = subquery.where
  end

  test "complex negative conditions in sub-query having" do
    query = make_query!("""
      select count(*) from
        (select user_id from lcf_conditions group by user_id having count(round(x)) <> 1 and count(round(x)) <> 5) as t
    """)
    {:subquery, %{ast: subquery}} = query.from
    assert {:comparison, _, :<>, %Expression{value: 1}} = subquery.having
  end

  describe "dropping constants in IN" do
    test "a single constant has too few matching users" do
      :ok = insert_rows(_user_ids = 11..20, "lcf_conditions", ["x"], [151])
      :ok = insert_rows(_user_ids = 21..30, "lcf_conditions", ["x"], [152])
      :ok = insert_rows(_user_ids = 31..31, "lcf_conditions", ["x"], [153])

      assert_query "select count(*) from lcf_conditions where x in (151, 152)",
        %{rows: [%{row: [count1]}]}
      assert_query "select count(*) from lcf_conditions where x in (151, 152, 153)",
        %{rows: [%{row: [count2]}]}
      assert count1 == count2
    end

    test "all constants have too few matching users" do
      for user_id <- 1..5, do:
        :ok = insert_rows(_user_ids = user_id..user_id, "lcf_conditions", ["x"], [150 + user_id])

      assert_query "select count(*) from lcf_conditions where x in (151, 152, 153, 154, 155)",
        %{rows: [%{row: [0]}]}
    end

    test "constants are checked in the context of the original query" do
      :ok = insert_rows(_user_ids = 11..20, "lcf_conditions", ["x", "y"], [151, 1])
      :ok = insert_rows(_user_ids = 21..22, "lcf_conditions", ["x", "y"], [152, 2])
      :ok = insert_rows(_user_ids = 23..25, "lcf_conditions", ["x", "y"], [152, 1])

      assert_query "select count(*) from lcf_conditions where x in (151, 152) and y <> 2",
        %{rows: [%{row: [10]}]}
    end
  end
end
