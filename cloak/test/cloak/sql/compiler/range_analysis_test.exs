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

    property "range can be computed for simplest arguments to function" do
      check all {name, function} <- function() do
        arity = Function.info(function) |> Keyword.fetch!(:arity)
        args = 1..arity |> Enum.map(fn _ -> column_in_range({2, 2}) end)
        expression = function_expression(name, args)
        assert RangeAnalysis.analyze_expression(expression).range != :unknown
      end
    end

    property "expression result is within computed range" do
      check all {name, function} <- function(),
                ranges <- list_of(range(), length: Function.info(function) |> Keyword.fetch!(:arity)),
                values <- values(ranges) do
        expression = function_expression(name, Enum.map(ranges, &column_in_range/1))

        case RangeAnalysis.analyze_expression(expression).range do
          :unknown ->
            :ok

          {min, max} ->
            result = apply(function, values)
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
      constant({"+", &Kernel.+/2}),
      constant({"-", &Kernel.-/2}),
      constant({"*", &Kernel.*/2}),
      constant({"abs", &Kernel.abs/1})
    ])
  end

  defp range() do
    gen all a <- integer(), b <- integer() do
      {min(a, b), max(a, b)}
    end
  end

  defp values(ranges) do
    Enum.reduce(ranges, constant([]), fn range, acc_generator ->
      bind(acc_generator, fn acc_value ->
        value(range)
        |> map(fn range_value ->
          [range_value | acc_value]
        end)
      end)
    end)
    |> map(&Enum.reverse/1)
  end

  defp value({min, max}), do: integer(min..max)

  defp table(), do: %{keys: %{}}
end
