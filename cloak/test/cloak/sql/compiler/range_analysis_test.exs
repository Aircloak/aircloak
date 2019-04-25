defmodule Cloak.Sql.Compiler.RangeAnalysis.Test do
  use ExUnit.Case

  alias Cloak.Sql.Compiler.RangeAnalysis
  alias Cloak.Sql.Expression

  describe ".analyze_expression" do
    test "integer constants" do
      assert {2, 2} = RangeAnalysis.analyze_expression(Expression.constant(:integer, 2)).range
    end

    test "real constants" do
      assert {2, 2} = RangeAnalysis.analyze_expression(Expression.constant(:real, 2)).range
    end

    test "other constants" do
      assert :unknown = RangeAnalysis.analyze_expression(Expression.constant(:text, "Some text")).range
    end
  end
end
