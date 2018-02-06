defmodule Cloak.Sql.Query.Lenses.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Query
  alias Cloak.Sql.Query.Lenses

  @no_location {1, 1}

  describe "terminals" do
    test "returns expressions" do
      query = %Query{columns: [:expression]}
      assert [:expression] == Lenses.terminals() |> normalize_elements(query)
    end

    test "focuses on function arguments as well as function" do
      query = %Query{columns: [{:function, "name", [:args], @no_location}]}
      assert [:args, {:function, "name", [:args], @no_location}] == Lenses.terminals() |> normalize_elements(query)
    end

    test "recurses inside aliases" do
      query = %Query{columns: [{{:function, "name", [:args], @no_location}, :as, "alias"}]}
      assert [:args, {:function, "name", [:args], @no_location}] == Lenses.terminals() |> normalize_elements(query)
    end
  end

  defp normalize_elements(lens, query), do:
    lens |> Lens.to_list(query) |> Enum.filter(& &1) |> Enum.sort()

  describe "join_condition_lenses" do
    test "a simple join" do
      query = %{from: {:join, %{lhs: "lhs", rhs: "rhs", conditions: :simple_conditions}}}
      lenses = Lenses.join_condition_lenses(query)

      assert [:simple_conditions] = Enum.map(lenses, &Lens.one!(&1, query))
    end

    test "a complex join" do
      query = %{from: {:join, %{
         lhs: {:join, %{lhs: "lhs", rhs: "rhs", conditions: :conditions2}},
         rhs: {:join, %{lhs: "lhs", rhs: "rhs", conditions: :conditions3}},
         conditions: :conditions1
       }}}
      lenses = Lenses.join_condition_lenses(query)

      assert [:conditions1, :conditions2, :conditions3] = Enum.map(lenses, &Lens.one!(&1, query))
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
      query = %{from: {:subquery, %{ast: %{
        from: {:join, %{
          lhs: {:subquery, %{ast: %{from: "table1"}}},
          rhs: {:subquery, %{ast: %{from: {:subquery, %{ast: %{from: "table2"}}}}}}}}}}}}

      assert [
        %{from: "table1"},
        %{from: "table2"},
        %{from: {:subquery, %{ast: %{from: "table2"}}}},
        %{
          from: {:join, %{
            lhs: {:subquery, %{ast: %{from: "table1"}}},
            rhs: {:subquery, %{ast: %{from: {:subquery, %{ast: %{from: "table2"}}}}}}}}
        },
        ^query,
      ] = Lens.to_list(Lenses.all_queries(), query)
    end
  end
end
