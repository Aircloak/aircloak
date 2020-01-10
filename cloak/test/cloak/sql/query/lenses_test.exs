defmodule Cloak.Sql.Query.Lenses.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Query
  alias Cloak.Sql.Query.Lenses

  describe "terminals" do
    test "returns expressions" do
      query = %Query{columns: [:expression]}
      assert [:expression] == Lenses.terminals() |> normalize_elements(query)
    end

    test "focuses on function arguments as well as function" do
      query = %Query{columns: [{:function, "name", [:args], nil}]}

      assert [:args, {:function, "name", [:args], nil}] == Lenses.terminals() |> normalize_elements(query)
    end

    test "recurses inside aliases" do
      query = %Query{columns: [{{:function, "name", [:args], nil}, :as, "alias"}]}

      assert [:args, {:function, "name", [:args], nil}] == Lenses.terminals() |> normalize_elements(query)
    end
  end

  defp normalize_elements(lens, query), do: lens |> Lens.to_list(query) |> Enum.filter(& &1) |> Enum.sort()

  describe "join_condition_lenses" do
    test "a simple join" do
      query = %{from: {:join, %{lhs: "lhs", rhs: "rhs", condition: :simple_condition}}}
      lenses = Lenses.join_condition_lenses(query)

      assert [:simple_condition] = Enum.map(lenses, &Lens.one!(&1, query))
    end

    test "a complex join" do
      query = %{
        from:
          {:join,
           %{
             lhs: {:join, %{lhs: "lhs", rhs: "rhs", condition: :condition2}},
             rhs: {:join, %{lhs: "lhs", rhs: "rhs", condition: :condition3}},
             condition: :condition1
           }}
      }

      lenses = Lenses.join_condition_lenses(query)

      assert [:condition1, :condition2, :condition3] = Enum.map(lenses, &Lens.one!(&1, query))
    end
  end

  describe "all_queries" do
    test "no subqueries" do
      query = %{from: "table"}
      assert [^query] = get_in(query, [Lenses.all_queries()])
    end

    test "a single subquery" do
      query = %{from: {:subquery, %{ast: %{from: "table"}}}}
      assert [%{from: "table"}, ^query] = get_in(query, [Lenses.all_queries()])
    end

    test "nested subqueries" do
      query = %{
        from:
          {:subquery,
           %{
             ast: %{
               from:
                 {:join,
                  %{
                    lhs: {:subquery, %{ast: %{from: "table1"}}},
                    rhs: {:subquery, %{ast: %{from: {:subquery, %{ast: %{from: "table2"}}}}}}
                  }}
             }
           }}
      }

      assert [
               %{from: "table1"},
               %{from: "table2"},
               %{from: {:subquery, %{ast: %{from: "table2"}}}},
               %{
                 from:
                   {:join,
                    %{
                      lhs: {:subquery, %{ast: %{from: "table1"}}},
                      rhs: {:subquery, %{ast: %{from: {:subquery, %{ast: %{from: "table2"}}}}}}
                    }}
               },
               ^query
             ] = Lens.to_list(Lenses.all_queries(), query)
    end
  end
end
