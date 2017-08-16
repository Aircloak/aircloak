defmodule Cloak.Query.ProbeTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers

  alias Cloak.Sql.Expression
  alias Cloak.Query.Probe

  setup_all do
    :ok = Cloak.Test.DB.create_table("lcf_conditions", "x INTEGER")
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
end
