defmodule Cloak.Sql.Compiler.Narrative.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.{Compiler.TypeChecker.Narrative, Expression}
  alias Cloak.DataSource.Table

  describe "construct_implicit_range_narrative" do
    test "multiple implicit ranges can't be combined" do
      narrative = Narrative.construct_implicit_range_narrative(["ir1", "ir2"], [], [expression("foo", "bar")])
      assert narrative =~ ~r/multiple functions/i
      assert narrative =~ ~r/ir1/
      assert narrative =~ ~r/ir2/
    end

    test "mentions offending functions" do
      narrative = Narrative.construct_implicit_range_narrative(["ir"], ["fn1"], [expression("foo", "bar")])
      assert narrative =~ ~r/in combination with other functions/i
      assert narrative =~ ~r/fn1/
    end

    test "mentions column it relates to" do
      narrative = Narrative.construct_implicit_range_narrative(["ir"], ["fn1"], [expression("foo", "bar")])
      assert narrative =~ ~r/`foo` from table `bar`/
    end

    test "explicit ranges" do
      narrative = Narrative.construct_implicit_range_narrative([], ["fn1"], [expression("foo", "bar")])
      assert narrative =~ ~r/`foo` from table `bar`/
    end
  end

  defp expression(name, table_name), do:
    %Expression{name: name, table: Table.new(table_name, "not-set")}
end
