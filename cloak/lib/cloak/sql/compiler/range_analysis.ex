defmodule Cloak.Sql.Compiler.RangeAnalysis do
  alias Cloak.Sql.{Expression, Query}

  def analyze_expression(expression),
    do: update_in(expression, [Query.Lenses.all_expressions()], &do_analyze_expression/1)

  defp do_analyze_expression(expression = %Expression{type: type, constant?: true, value: value})
       when type in [:integer, :real],
       do: %{expression | range: {value, value}}

  defp do_analyze_expression(expression = %Expression{constant?: false, function?: false}),
    do: expression

  defp do_analyze_expression(expression = %Expression{function?: true, function: name, function_args: args}),
    do: %{expression | range: update_range(name, Enum.map(args, & &1.range))}

  defp do_analyze_expression(expression), do: %{expression | range: :unknown}

  defp update_range("+", [{min1, max1}, {min2, max2}]), do: {min1 + min2, max1 + max2}
  defp update_range("-", [{min1, max1}, {min2, max2}]), do: {min1 - max2, max1 - min2}

  defp update_range("*", [{min1, max1}, {min2, max2}]) do
    options = [min1 * min2, max1 * max2, min1 * max2, min2 * max1]
    {Enum.min(options), Enum.max(options)}
  end

  defp update_range("abs", [{min, max}]), do: {max(min, 0), max(abs(min), abs(max))}
  defp update_range(fun, [range]) when fun in ["floor", "ceil", "round", "trunc"], do: range
  defp update_range(_, _), do: :unknown
end
