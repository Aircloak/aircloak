defmodule Cloak.Sql.Compiler.RangeAnalysis.Test do
  use ExUnit.Case
  use ExUnitProperties

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

    test "columns are ignored" do
      assert {10, 20} = RangeAnalysis.analyze_expression(column_in_range({10, 20})).range
    end

    property "expression result is within computed range" do
      check all {name, function} <- function(),
                range1 <- range(),
                value1 <- value(range1),
                range2 <- range(),
                value2 <- value(range2) do
        expression = function_expression(name, [column_in_range(range1), column_in_range(range2)])

        case RangeAnalysis.analyze_expression(expression).range do
          :unknown ->
            :ok

          {min, max} ->
            result = function.(value1, value2)
            assert result <= max
            assert result >= min
        end
      end
    end
  end

  defp column_in_range(range) do
    %{Expression.column(%{name: "column", type: "real"}, table()) | range: range}
  end

  defp function_expression(function_name, args) do
    Expression.function(function_name, args, :real)
  end

  defp function() do
    one_of([
      constant({"+", &Kernel.+/2})
    ])
  end

  defp range() do
    gen all a <- integer(), b <- integer() do
      {min(a, b), max(a, b)}
    end
  end

  defp value({min, max}), do: integer(min..max)

  defp table(), do: %{keys: %{}}
end
