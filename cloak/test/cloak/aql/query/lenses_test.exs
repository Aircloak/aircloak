defmodule Cloak.Aql.Query.Lenses.Test do
  use ExUnit.Case, async: true

  alias Cloak.Aql.Query.Lenses

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
end
