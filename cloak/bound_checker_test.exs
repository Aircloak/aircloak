defmodule Cloak.SQL.Compiler.BoundChecker.Test do
  use ExUnit.Case

  alias Cloak.Sql.Compiler.BoundChecker
  alias Cloak.Sql.Expression

  @max_int 9_223_372_036_854_775_807

  describe ".check_expression" do
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

    test "/ with unknown bounds on dividend" do
      dividend = in_bounds(:unknown)
      divisor = in_bounds({10, 20})

      assert %Expression{function: "/", function_args: [^dividend, ^divisor]} =
               BoundChecker.check_expression(function("/", [dividend, divisor]))
    end

    test "/ with unknown bounds on divisor" do
      dividend = in_bounds({10, 20})
      divisor = in_bounds(:unknown)

      assert %Expression{function: "/", function_args: [^dividend, ^divisor]} =
               BoundChecker.check_expression(function("/", [dividend, divisor]))
    end

    test "% with divisor spanning 0" do
      expression =
        %Expression{function: "%", function_args: [in_bounds({10, 20}), in_bounds({-10, 10})]}
        |> in_bounds({0, 100})

      assert BoundChecker.check_expression(expression) == %{expression | function: "checked_mod"}
    end

    test "% with divisor not spanning 0" do
      expression =
        %Expression{function: "%", function_args: [in_bounds({10, 20}), in_bounds({-100, -10})]}
        |> in_bounds({0, 100})

      assert BoundChecker.check_expression(expression) == %{expression | function: "unsafe_mod"}
    end

    test "% with too large output bounds" do
      expression =
        %Expression{function: "%", function_args: [in_bounds({10, 20}), in_bounds({-100, -10})]}
        |> in_bounds({0, @max_int + 1})

      assert BoundChecker.check_expression(expression) == expression
    end

    test "integer expression with result within 64bit unsigned bounds" do
      a = in_bounds({10, 20})
      expression = function("+", [a, a]) |> in_bounds({100, 200})
      assert %Expression{function: "unsafe_add", function_args: [^a, ^a]} = BoundChecker.check_expression(expression)
    end

    test "integer expression with result outside of 64bit unsigned bounds" do
      a = in_bounds({10, 20})
      expression = function("+", [a, a]) |> in_bounds({0, @max_int + 1})
      assert ^expression = BoundChecker.check_expression(expression)
    end

    test "real expression with magnitude of result smaller than 1.0e100" do
      a = in_bounds({10, 20})
      expression = function("+", [a, a], :real) |> in_bounds({0, 1.0e50})
      assert %Expression{function: "unsafe_add", function_args: [^a, ^a]} = BoundChecker.check_expression(expression)
    end

    test "real expression with magnitude of result larger than 1.0e100" do
      a = in_bounds({10, 20})
      expression = function("+", [a, a], :real) |> in_bounds({0, 1.0e101})
      assert ^expression = BoundChecker.check_expression(expression)
    end
  end

  defp in_bounds(expression \\ Expression.constant(:integer, 0), bounds) do
    put_in(expression, [Lens.key(:bounds)], bounds)
  end

  defp function(name, args, type \\ :integer) do
    Expression.function(name, args, type)
  end
end
