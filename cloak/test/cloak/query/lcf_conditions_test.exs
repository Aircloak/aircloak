defmodule Cloak.Query.LCFConditionsTest do
  use ExUnit.Case, async: true

  import Cloak.Test.QueryHelpers
  import Lens.Macros

  alias Cloak.Sql.{Compiler, Parser, Query, Expression}
  alias Cloak.Query.LCFConditions

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

  defp build_query(query_string) do
    parsed_query = Parser.parse!(query_string)
    # compile parsed query with first data source
    [first_ds | rest_ds] = Cloak.DataSource.all()
    query = compile_query(parsed_query, first_ds)
    # make sure responses from all data_sources are equal
    for data_source <- rest_ds, do: assert(query == compile_query(parsed_query, data_source))
    query
  end

  defp compile_query(parsed_query, data_source) do
    {:ok, query} = Compiler.compile(data_source, parsed_query, [], %{})
    query |> LCFConditions.process() |> scrub_data_sources() |> scrub_aliases()
  end

  defp scrub_data_sources(query), do:
    put_in(query, [all_subqueries() |> Lens.key(:data_source)], nil)

  defp scrub_aliases(query), do:
    put_in(query, [all_subqueries() |> Query.Lenses.query_expressions() |> Lens.key(:alias)], nil)

  deflens all_subqueries(), do:
    Lens.both(Lens.recur(Query.Lenses.direct_subqueries() |> Lens.key(:ast)), Lens.root())

  test "complex negative conditions in top query" do
    query = build_query("select count(*) from lcf_conditions where round(x) <> 2 and round(x) <> 0")
    assert {:and, {:not, {:is, _, :null}}, {:comparison, _, :<>, %Expression{value: 0}}} = query.where
  end

  test "complex negative conditions in sub-query where" do
    query = build_query("""
      select count(*) from (select user_id from lcf_conditions where round(x) <> 2 and round(x) <> 0) as t
    """)
    {:subquery, %{ast: subquery}} = query.from
    assert {:comparison, _, :<>, %Expression{value: 0}} = subquery.where
  end

  test "complex negative conditions in sub-query having" do
    query = build_query("""
      select count(*) from
        (select user_id from lcf_conditions group by user_id having count(round(x)) <> 1 and count(round(x)) <> 5) as t
    """)
    {:subquery, %{ast: subquery}} = query.from
    assert {:comparison, _, :<>, %Expression{value: 1}} = subquery.having
  end
end
