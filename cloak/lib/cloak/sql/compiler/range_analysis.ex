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

  defp update_range("*", [{min1, max1}, {min2, max2}]),
    do: [min1 * min2, max1 * max2, min1 * max2, min2 * max1] |> Enum.min_max()

  defp update_range("abs", [{min, max}]), do: {max(min, 0), max(abs(min), abs(max))}
  defp update_range("sqrt", [{_min, max}]) when max >= 0, do: {0, max |> :math.sqrt() |> ceil()}

  defp update_range("^", [{min1, max1}, {min2, max2}]) do
    if min1 < 0 and max1 > -1 and min2 < 0 do
      :unknown
    else
      base = if max2 < 0, do: min(abs(min1), abs(min2)), else: max(abs(min1), abs(max1))
      extent = :math.pow(base, max2)
      {floor(-extent), ceil(extent)}
    end
  rescue
    ArithmeticError -> :unknown
  end

  defp update_range("/", [{min1, max1}, {min2, max2}]) do
    if min2 <= 0 and max2 >= 0 do
      :unknown
    else
      options = [min1 / min2, min1 / max2, max1 / min2, max1 / max2]
      {options |> Enum.min() |> floor(), options |> Enum.max() |> ceil()}
    end
  end

  defp update_range(fun, [range]) when fun in ["floor", "ceil", "round", "trunc"], do: range
  defp update_range(_, _), do: :unknown

  defp floor(number), do: number |> :math.floor() |> round()
  defp ceil(number), do: number |> :math.ceil() |> round()
end
