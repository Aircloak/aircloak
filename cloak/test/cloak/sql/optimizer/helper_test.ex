defmodule Cloak.Sql.Optimizer.Helper.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Optimizer.Helper

  describe "eligible" do
    test "queries for a single table are eligible" do
      assert Helper.eligible(default_query())
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
      |> with_columns([column({:unquoted, "age"})])
      |> Helper.eligible()
    end

    Enum.each(["count", "sum", "min", "max"], fn(aggregate_function) ->
      test "allow supported aggregates (#{aggregate_function})" do
        assert default_query()
        |> with_columns([aggregate(unquote(aggregate_function))])
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
  end

  describe "user_id_column" do
    test "returns user id for table" do
      data_source = %{tables: %{test: %{columns: [], user_id: "uid"}}}
      assert {:ok, {:unquoted, "uid"}} == Helper.user_id_column({:unquoted, "test"}, data_source)
    end

    test "returns not found when no match" do
      data_source = %{tables: %{test: %{columns: [], user_id: "uid"}}}
      assert :not_found == Helper.user_id_column({:unquoted, "other"}, data_source)
    end
  end

  defp default_query(), do:
    query()
    |> with_columns([column({:unquoted, "age"}), count()])
    |> with_table({:unquoted, "table"})
    |> with_group_by([column({:unquoted, "age"})])

  defp count(target \\ "*"), do: aggregate("count", target)

  defp aggregate(function, target \\ "*"), do: {:function, function, [target]}

  defp column(name), do: {:identifier, :unknown, name}

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
end
