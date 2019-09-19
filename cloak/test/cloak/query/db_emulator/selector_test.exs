defmodule Cloak.Query.DbEmulator.Selector.Test do
  use ExUnit.Case, async: true

  alias Cloak.Query.DbEmulator.Selector
  alias Cloak.Sql.{Expression, Query}

  defp column(name), do: %Expression{kind: :column, type: :integer, name: name, table: :unknown}

  describe "pick_db_columns" do
    test "columns from subquery" do
      query = %Query{
        db_columns: [column("column1"), column("column2")],
        from: {:subquery, %{ast: %{column_titles: ["column1", "something else", "column2"]}}}
      }

      rows = [[:data1, :data_to_ignore, :data2]]

      assert [[:data1, :data2]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end

    test "columns from join" do
      query = %Query{
        db_columns: [column("column1"), column("column2")],
        from:
          {:join,
           %{
             columns: [
               column("column1"),
               column("something else"),
               column("column2")
             ]
           }}
      }

      rows = [[:data1, :data_to_ignore, :data2]]

      assert [[:data1, :data2]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end

    test "handling complex expressions" do
      query = %Query{
        db_columns: [
          Expression.function("+", [column("column1"), column("column2")], :integer)
        ],
        from: {:subquery, %{ast: %{column_titles: ["column1", "something else", "column2"]}}}
      }

      rows = [[1, :data_to_ignore, 2]]

      assert [[3]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end

    test "handling out of order indexing" do
      query = %Query{
        db_columns: [column("column2"), column("column1")],
        from: {:subquery, %{ast: %{column_titles: ["column1", "something else", "column2"]}}}
      }

      rows = [[:data1, :data_to_ignore, :data2]]

      assert [[:data2, :data1]] = Selector.pick_db_columns(rows, query) |> Enum.to_list()
    end
  end
end
