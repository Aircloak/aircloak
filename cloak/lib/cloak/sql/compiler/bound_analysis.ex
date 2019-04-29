defmodule Cloak.Sql.Compiler.BoundAnalysis do
  @dummy_bounds {10, 20}

  alias Cloak.Sql.{Expression, Query}
  alias Cloak.Sql.Compiler.Helpers

  def analyze_query(query) do
    Helpers.apply_bottom_up(query, fn subquery ->
      subquery
      |> propagate_subquery_bounds()
      |> update_in([Query.Lenses.query_expressions()], &analyze_expression/1)
    end)
  end

  def analyze_expression(expression),
    do: update_in(expression, [Query.Lenses.all_expressions()], &do_analyze_expression/1)

  defp propagate_subquery_bounds(query) do
    update_in(
      query,
      [Query.Lenses.query_expressions() |> Query.Lenses.leaf_expressions() |> Lens.reject(&Expression.constant?/1)],
      fn expression ->
        case Query.resolve_subquery_column(expression, query) do
          :database_column -> expression
          {column, _subquery} -> %{expression | bounds: column.bounds}
        end
      end
    )
  end

  defp do_analyze_expression(expression = %Expression{type: type, constant?: true, value: value})
       when type in [:integer, :real],
       do: %{expression | bounds: {value, value}}

  defp do_analyze_expression(expression = %Expression{constant?: false, function?: false, bounds: :unknown}),
    do: %{expression | bounds: @dummy_bounds}

  defp do_analyze_expression(expression = %Expression{constant?: false, function?: false}),
    do: expression

  defp do_analyze_expression(expression = %Expression{function?: true, function_args: [:*]}),
    do: expression

  defp do_analyze_expression(expression = %Expression{function?: true, function: name, function_args: args}),
    do: %{expression | bounds: update_bounds(name, Enum.map(args, & &1.bounds))}

  defp do_analyze_expression(expression), do: %{expression | bounds: :unknown}

  defp update_bounds("+", [{min1, max1}, {min2, max2}]), do: {min1 + min2, max1 + max2}
  defp update_bounds("-", [{min1, max1}, {min2, max2}]), do: {min1 - max2, max1 - min2}

  defp update_bounds("*", [{min1, max1}, {min2, max2}]),
    do: [min1 * min2, max1 * max2, min1 * max2, min2 * max1] |> Enum.min_max()

  defp update_bounds("abs", [{min, max}]), do: {max(min, 0), max(abs(min), abs(max))}
  defp update_bounds("sqrt", [{_min, max}]) when max >= 0, do: {0, max |> :math.sqrt() |> ceil()}

  defp update_bounds("^", [{min1, max1}, {min2, max2}]) do
    if min1 <= 0 and max1 >= 0 and min2 < 0 do
      :unknown
    else
      base = if max2 < 0, do: min(abs(min1), abs(min2)), else: max(abs(min1), abs(max1))
      extent = :math.pow(base, max2)
      {floor(-extent), max(ceil(extent), 1)}
    end
  rescue
    ArithmeticError -> :unknown
  end

  defp update_bounds("/", [{min1, max1}, {min2, max2}]) do
    if min2 <= 0 and max2 >= 0 do
      :unknown
    else
      options = [min1 / min2, min1 / max2, max1 / min2, max1 / max2]
      {options |> Enum.min() |> floor(), options |> Enum.max() |> ceil()}
    end
  end

  defp update_bounds("%", [_, {min, max}]) do
    divisor = max(abs(min), abs(max))
    {-divisor, divisor}
  end

  defp update_bounds(fun, [bounds]) when fun in ["floor", "ceil", "round", "trunc"], do: bounds
  defp update_bounds(_, _), do: :unknown

  defp floor(number), do: number |> :math.floor() |> round()
  defp ceil(number), do: number |> :math.ceil() |> round()
end
