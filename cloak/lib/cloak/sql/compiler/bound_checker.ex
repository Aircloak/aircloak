defmodule Cloak.Sql.Compiler.BoundChecker do
  alias Cloak.Sql.{Query, Expression}

  @large_float_number 1.0e100
  @max_int 9_223_372_036_854_775_807
  @unsafe_names %{
    "+" => "unsafe_add",
    "-" => "unsafe_sub",
    "/" => "unsafe_div",
    "*" => "unsafe_mul",
    "^" => "unsafe_pow",
    "%" => "unsafe_mod"
  }

  def check_expression(expression) do
    expression
    |> check_division()
    |> check_functions()
  end

  defp check_functions(expression) do
    update_in(
      expression,
      [Query.Lenses.all_expressions() |> Lens.filter(&(&1.function in Map.keys(@unsafe_names)))],
      fn expression = %Expression{function: name, type: type, bounds: bounds} ->
        if within_bounds?(type, bounds) do
          %{expression | function: Map.fetch!(@unsafe_names, name)}
        else
          expression
        end
      end
    )
  end

  defp within_bounds?(:integer, {min, max}), do: min > -@max_int && max < @max_int
  defp within_bounds?(:real, {min, max}), do: min > -@large_float_number && max < @large_float_number
  defp within_bounds?(_, _), do: false

  defp check_division(expression) do
    update_in(expression, [Query.Lenses.all_expressions() |> Lens.filter(&(&1.function == "/"))], &do_check_division/1)
  end

  defp do_check_division(expression = %Expression{function: "/", function_args: [%{bounds: :unknown}, _]}),
    do: expression

  defp do_check_division(expression = %Expression{function: "/", function_args: [_, %{bounds: :unknown}]}),
    do: expression

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
    if magnitude < @large_float_number, do: {:ok, magnitude / @large_float_number}, else: :error
  rescue
    _ -> :error
  end
end
