defmodule Cloak.SQL.Compiler.BoundChecker.Test do
  use ExUnit.Case

  alias Cloak.Sql.Compiler.BoundChecker
  alias Cloak.Sql.Expression

  test "/ with safe argument bounds results in an unsafe_div" do
    dividend = in_bounds({10, 20})
    divisor = in_bounds({10, 20})

    assert %Expression{function: "unsafe_div", function_args: [^dividend, ^divisor]} =
             BoundChecker.check_expression(function("/", [dividend, divisor]))
  end

  test "/ with reasonable argument bounds results in a checked_div" do
    dividend = in_bounds({10, 20})
    divisor = in_bounds({-10, 10})

    assert %Expression{function: "checked_div", function_args: [^dividend, ^divisor, epsilon]} =
             BoundChecker.check_expression(function("/", [dividend, divisor]))

    assert epsilon < 1
    assert 20 / epsilon < 1.0e101
  end

  test "/ with unreasonable argument bounds results in a /" do
    dividend = in_bounds({round(-1.0e200), round(1.0e200)})
    divisor = in_bounds({-1, 1})

    assert %Expression{function: "/", function_args: [^dividend, ^divisor]} =
             BoundChecker.check_expression(function("/", [dividend, divisor]))
  end

  test "with unknown bounds on dividend"

  test "with unknown bounds on divisor"

  test "with {0, 0} range on divisor"

  defp in_bounds(bounds) do
    Expression.constant(:integer, 0) |> put_in([Lens.key(:bounds)], bounds)
  end

  defp function(name, args) do
    Expression.function(name, args, :integer)
  end
end
