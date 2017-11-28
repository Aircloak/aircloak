defmodule Cloak.Sql.Query.Lenses.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Query
  alias Cloak.Sql.Query.Lenses

  describe "terminals" do
    test "returns expressions" do
      query = %Query{columns: [:expression]}
      assert [:expression] == Lenses.terminals() |> normalize_elements(query)
    end

    test "focuses on function arguements as well as function" do
      query = %Query{columns: [{:function, "name", [:args]}]}
      assert [:args, {:function, "name", [:args]}] == Lenses.terminals() |> normalize_elements(query)
    end

    test "recurses inside aliases" do
      query = %Query{columns: [{{:function, "name", [:args]}, :as, "alias"}]}
      assert [:args, {:function, "name", [:args]}] == Lenses.terminals() |> normalize_elements(query)
    end
  end

  defp normalize_elements(lens, query), do:
    lens |> Lens.to_list(query) |> Enum.filter(& &1) |> Enum.sort()

  describe "join_condition_lenses" do
    test "a simple join" do
      query = %{from: {:join, %{lhs: "lhs", rhs: "rhs", conditions: :simple_conditions}}}
      lenses = Lenses.join_condition_lenses(query)

      assert [:simple_conditions] = Enum.map(lenses, &Lens.get(&1, query))
    end

    test "a complex join" do
      query = %{from: {:join, %{
         lhs: {:join, %{lhs: "lhs", rhs: "rhs", conditions: :conditions2}},
         rhs: {:join, %{lhs: "lhs", rhs: "rhs", conditions: :conditions3}},
         conditions: :conditions1
       }}}
      lenses = Lenses.join_condition_lenses(query)

      assert [:conditions1, :conditions2, :conditions3] = Enum.map(lenses, &Lens.get(&1, query))
    end
  end

  describe "subqueries" do
    test "a single subquery" do
      query = %{from: {:subquery, %{ast: %{from: "table"}}}}

      assert %{ast: %{from: "table"}} = Lens.get(Lenses.subqueries(), query)
    end

    test "nested subqueries" do
      query = %{from: {:subquery, %{ast: %{
        from: {:join, %{
          lhs: {:subquery, %{ast: %{from: "table1"}}},
          rhs: {:subquery, %{ast: %{from: {:subquery, %{ast: %{from: "table2"}}}}}}}}}}}}

      assert [
        %{ast: %{
          from: {:join, %{
            lhs: {:subquery, %{ast: %{from: "table1"}}},
            rhs: {:subquery, %{ast: %{from: {:subquery, %{ast: %{from: "table2"}}}}}}}}
        }},
        %{ast: %{from: {:subquery, %{ast: %{from: "table2"}}}}},
        %{ast: %{from: "table1"}},
        %{ast: %{from: "table2"}},
      ] = Lens.get(Lenses.subqueries(), query) |> Enum.sort()
    end
  end
end
