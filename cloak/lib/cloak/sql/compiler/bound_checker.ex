defmodule Cloak.Sql.Compiler.BoundChecker do
  alias Cloak.Sql.{Query, Expression}

  @large_number 1.0e100

  def check_expression(expression) do
    expression
    |> check_division()
  end

  defp check_division(expression) do
    update_in(expression, [Query.Lenses.all_expressions() |> Lens.filter(&(&1.function == "/"))], &do_check_division/1)
  end

  defp do_check_division(expression = %Expression{function: "/", function_args: [dividend, divisor]}) do
    {min, max} = divisor.bounds

    if min > 0 || max < 0 do
      expression
    else
      %{expression | function: "checked_div", function_args: [dividend, divisor, div_epsilon(dividend)]}
    end
  end

  defp div_epsilon(%{bounds: {min, max}}) do
    max(abs(min), abs(max)) / @large_number
  end
end
