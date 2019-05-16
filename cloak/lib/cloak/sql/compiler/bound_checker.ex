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
    case {spans_zero?(divisor.bounds), div_epsilon(dividend)} do
      {false, _} ->
        %{expression | function: "unsafe_div"}

      {_, {:ok, epsilon}} ->
        %{expression | function: "checked_div", function_args: [dividend, divisor, epsilon]}

      _ ->
        expression
    end
  end

  defp spans_zero?({min, max}), do: min < 1 && max > -1

  defp div_epsilon(%{bounds: {min, max}}) do
    magnitude = max(abs(min), abs(max))
    if magnitude < @large_number, do: {:ok, magnitude / @large_number}, else: :error
  rescue
    _ -> :error
  end
end
