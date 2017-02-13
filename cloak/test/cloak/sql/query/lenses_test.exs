defmodule Cloak.Sql.Query.Lenses.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Query.Lenses

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
      query = %{from: {:subquery, %{from: "table"}}}

      assert %{from: "table"} = Lens.get(Lenses.subqueries(), query)
    end

    test "nested subqueries" do
      query = %{from: {:subquery, %{
        from: {:join, %{
          lhs: {:subquery, %{from: "table1"}},
          rhs: {:subquery, %{from: {:subquery, %{from: "table2"}}}}}}}}}

      assert [
        %{
          from: {:join, %{
            lhs: {:subquery, %{from: "table1"}},
            rhs: {:subquery, %{from: {:subquery, %{from: "table2"}}}}}}
        },
        %{from: {:subquery, %{from: "table2"}}},
        %{from: "table1"},
        %{from: "table2"},
      ] = Lens.get(Lenses.subqueries(), query) |> Enum.sort()
    end
  end
end
