defmodule Cloak.Query.DbEmulator.Selector.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.DbEmulator.Selector
  alias Cloak.Sql.{Expression, Query}

  describe "pick_db_columns" do
    test "reordering columns from subquery" do
      query = %Query{
        db_columns: [%Expression{name: "column1", row_index: 1}, %Expression{name: "column2", row_index: 0}],
        from: {:subquery, %{ast: %{column_titles: ["column1", "something else", "column2"]}}}
      }
      rows = [[:data1, :data_to_ignore, :data2]]

      assert [[:data2, :data1]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end

    test "reordering columns from join" do
      query = %Query{
        db_columns: [%Expression{name: "column1", row_index: 1}, %Expression{name: "column2", row_index: 0}],
        from: {:join, %{
          columns: [%Expression{name: "column1"}, %Expression{name: "something else"}, %Expression{name: "column2"}],
        }}
      }
      rows = [[:data1, :data_to_ignore, :data2]]

      assert [[:data2, :data1]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end

    test "handling complex expressions" do
      query = %Query{
        db_columns: [
          Expression.function("+", [%Expression{name: "column1"}, %Expression{name: "column2"}], row_index: 0)
        ],
        from: {:subquery, %{ast: %{column_titles: ["column1", "something else", "column2"]}}}
      }
      rows = [[1, :data_to_ignore, 2]]

      assert [[3]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end

    test "handling non-contiguous indexing" do
      query = %Query{
        db_columns: [%Expression{name: "column1", row_index: 3}, %Expression{name: "column2", row_index: 1}],
        from: {:subquery, %{ast: %{column_titles: ["column1", "something else", "column2"]}}}
      }
      rows = [[:data1, :data_to_ignore, :data2]]

      assert [[nil, :data2, nil, :data1]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end
  end
end
