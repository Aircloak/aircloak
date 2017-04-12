defmodule Cloak.Sql.Optimizer.Helper.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Optimizer.Helper

  describe "eligible" do
    test "queries for a single table are eligible" do
      assert Helper.eligible(default_query())
    end

    test "queries with only aggregate" do
      assert default_query()
      |> with_columns([count()])
      |> unset_group_by()
      |> Helper.eligible()
    end

    test "queries with JOINs are not eligible" do
      join_table = {:join, %{
        conditions: [{
          :comparison,
            {:identifier, {:unquoted, "customers"}, {:unquoted, "id"}}, :=,
            {:identifier, {:unquoted, "demographics"}, {:unquoted, "uid"}}
        }],
        lhs: {:unquoted, "demographics"},
        rhs: {:unquoted, "customers"},
        type: :inner_join
      }}
      refute default_query()
      |> with_table(join_table)
      |> Helper.eligible()
    end

    test "queries without aggregates are not eligible" do
      refute default_query()
      |> with_columns([column("age")])
      |> Helper.eligible()
    end

    test "deals with aliased functions" do
      assert default_query()
      |> with_columns([{count(), :as, "foo"}])
      |> with_group_by([])
      |> Helper.eligible()
    end

    Enum.each(["count", "sum", "min", "max"], fn(aggregate_function) ->
      test "allow supported aggregates (#{aggregate_function})" do
        assert default_query()
        |> with_columns([aggregate(unquote(aggregate_function))])
        |> with_group_by([])
        |> Helper.eligible()
      end
    end)

    Enum.each(["avg", "stddev"], fn(aggregate_function) ->
      test "forbid unsupported aggregates (#{aggregate_function})" do
        refute default_query()
        |> with_columns([aggregate(unquote(aggregate_function))])
        |> Helper.eligible()
      end
    end)

    test "rejects queries with non-aggregate functions" do
      refute default_query()
      |> with_columns([function("abs", column("age"))])
      |> Helper.eligible()
    end

    test "rejects queries with WHERE-clauses" do
      refute default_query()
      |> with_where([not: {
        :comparison, {:identifier, :unknown, {:unquoted, "amount"}},
        :=, {:constant, :integer, 10}
      }])
      |> Helper.eligible()
    end

    test "rejects when GROUP BY on non-selected column" do
      refute default_query()
      |> with_columns([count()])
      |> with_group_by(column("age"))
      |> Helper.eligible()
    end
  end

  describe "user_id_column" do
    test "returns user id for table" do
      data_source = %{tables: %{test: %{columns: [], user_id: "uid"}}}
      assert {:ok, column("uid")} == Helper.user_id_column({:unquoted, "test"}, data_source)
    end

    test "returns not found when no match" do
      data_source = %{tables: %{test: %{columns: [], user_id: "uid"}}}
      assert :not_found == Helper.user_id_column({:unquoted, "other"}, data_source)
    end
  end

  describe "column_replacement" do
    test "columns are replaced by itself" do
      column = column("name")
      {outer, inner} = Helper.column_replacement(column)
      assert {{:identifier, :unknown, {:unquoted, inner_alias1}}, :as, "name"} = outer
      assert {^column, :as, inner_alias2} = inner
      assert inner_alias1 == inner_alias2
    end

    test "columns retain their alias" do
      column = column("name")
      aliased_column = {column, :as, "alias"}
      {outer, inner} = Helper.column_replacement(aliased_column)
      assert {{:identifier, :unknown, {:unquoted, inner_alias1}}, :as, "alias"} = outer
      assert {^column, :as, inner_alias2} = inner
      assert inner_alias1 == inner_alias2
    end

    test "count(*) replaced with sum of counts" do
      {outer, inner} = Helper.column_replacement(count())
      assert {{:function, "sum", [{:identifier, :unknown, {:unquoted, inner_alias1}}]}, :as, "count"} = outer
      assert {{:function, "count", ["*"]}, :as, inner_alias2} = inner
      assert inner_alias1 == inner_alias2
    end

    Enum.each(["sum", "min", "max"], fn(aggregate_function) ->
      test "aggregate splits up (#{aggregate_function})" do
        {outer, inner} = Helper.column_replacement(
          aggregate(unquote(aggregate_function), column("age"))
        )
        assert {
          {:function, unquote(aggregate_function), [
            {:identifier, :unknown, {:unquoted, inner_alias1}}
          ]},
          :as, unquote(aggregate_function)
        } = outer
        assert {
          {:function, unquote(aggregate_function), [
            {:identifier, :unknown, {:unquoted, "age"}}
          ]},
          :as, inner_alias2
        } = inner
        assert inner_alias1 == inner_alias2
      end
    end)
  end

  defp default_query(), do:
    query()
    |> with_columns([column("age"), count()])
    |> with_table({:unquoted, "table"})
    |> with_group_by([column("age")])

  defp count(target \\ "*"), do: aggregate("count", target)

  defp aggregate(function, target \\ "*"), do: {:function, function, [target]}

  defp function(name, target), do: aggregate(name, target)

  defp column(name), do: {:identifier, :unknown, {:unquoted, name}}

  defp query(), do:
    %{
      columns: [], command: :select, distinct?: false,
      from: nil, group_by: []
    }

  defp with_columns(query, columns), do:
    Map.put(query, :columns, columns)

  defp with_table(query, table), do:
    Map.put(query, :from, table)

  defp with_group_by(query, group_by), do:
    Map.put(query, :group_by, group_by)

  defp with_where(query, where), do:
    Map.put(query, :where, where)

  defp unset_group_by(query), do:
    Map.delete(query, :group_by)
end
