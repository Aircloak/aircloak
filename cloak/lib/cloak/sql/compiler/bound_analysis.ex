defmodule Cloak.Sql.Compiler.BoundAnalysis do
  @moduledoc """
  Contains functions for analyzing the numerical bounds that expressions in a query take, given some input bounds for
  the columns. Currently all columns are set to the dummy bounds of {10, 20} and the analysis proceeds from there.
  """

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
  @divisions ~w(/ %)

  alias Cloak.Sql.{Expression, Query}
  alias Cloak.Sql.Compiler.Helpers
  alias Cloak.DataSource.Bounds

  use Lens.Macros

  # -------------------------------------------------------------------
  # API functions
  # -------------------------------------------------------------------

  @doc "Takes a query and sets the `bounds` for all of its expressions."
  @spec analyze_query(Query.t()) :: Query.t()
  def analyze_query(query) do
    Helpers.apply_bottom_up(query, fn subquery ->
      subquery
      |> set_leaf_bounds()
      |> update_in([Query.Lenses.query_expressions()], &set_bounds/1)
      |> update_in([Query.Lenses.query_expressions()], &analyze_safety/1)
    end)
  end

  @doc """
  Sets the `bounds` for the given expression.

  Assumes that bounds of leaf subexpressions are already present.
  """
  @spec set_bounds(Expression.t()) :: Expression.t()
  def set_bounds(expression),
    do: update_in(expression, [Query.Lenses.all_expressions()], &do_set_bounds/1)

  @doc """
  Changes the functions used in the expression to their unsafe or checked versions if possible.

  Assumes the bounds for all subexpressions are already set.
  """
  @spec analyze_safety(Expression.t()) :: Expression.t()
  def analyze_safety(expression) do
    expression
    |> check_division()
    |> check_pow()
    |> check_functions()
  end

  # -------------------------------------------------------------------
  # Bound computation
  # -------------------------------------------------------------------

  defp set_leaf_bounds(query) do
    update_in(query, [leaf_expressions()], fn expression ->
      case Query.resolve_subquery_column(expression, query) do
        :database_column ->
          %{expression | bounds: Bounds.bounds(query.data_source, expression.table.name, expression.name)}

        {column, _subquery} ->
          %{expression | bounds: column.bounds}
      end
    end)
  end

  deflensp leaf_expressions() do
    Query.Lenses.query_expressions() |> Query.Lenses.leaf_expressions() |> Lens.reject(&Expression.constant?/1)
  end

  defp do_set_bounds(expression = %Expression{constant?: true, value: value}) when value in [:*, nil],
    do: expression

  defp do_set_bounds(expression = %Expression{type: type, constant?: true, value: value})
       when type in [:integer, :real],
       do: %{expression | bounds: {floor(value), ceil(value)}}

  defp do_set_bounds(expression = %Expression{constant?: false, function?: false}),
    do: expression

  defp do_set_bounds(expression = %Expression{function?: true, function_args: [:*]}),
    do: expression

  defp do_set_bounds(expression = %Expression{function?: true, function_args: [{:distinct, _}]}),
    do: expression

  defp do_set_bounds(expression = %Expression{function: {:cast, to}, function_args: [%{bounds: bounds, type: from}]})
       when from in [:real, :integer] and to in [:real, :integer],
       do: %{expression | bounds: bounds}

  defp do_set_bounds(expression = %Expression{function: {:cast, to}, function_args: [%{type: :boolean}]})
       when to in [:real, :integer],
       do: %{expression | bounds: {0, 1}}

  defp do_set_bounds(expression = %Expression{function?: true, function: name, function_args: args}),
    do: %{expression | bounds: update_bounds(name, Enum.map(args, & &1.bounds))}

  defp do_set_bounds(expression), do: %{expression | bounds: :unknown}

  defp update_bounds("+", [{min1, max1}, {min2, max2}]), do: {min1 + min2, max1 + max2}
  defp update_bounds("-", [{min1, max1}, {min2, max2}]), do: {min1 - max2, max1 - min2}

  defp update_bounds("*", [{min1, max1}, {min2, max2}]),
    do: [min1 * min2, max1 * max2, min1 * max2, min2 * max1] |> Enum.min_max()

  defp update_bounds("abs", [{min, max}]), do: {max(min, 0), max(abs(min), abs(max))}

  defp update_bounds("sqrt", [{min, max}]) when min >= 0 and max >= 0,
    do: {min |> :math.sqrt() |> floor(), max |> :math.sqrt() |> ceil()}

  defp update_bounds("sqrt", [{_min, max}]) when max >= 0, do: {0, max |> :math.sqrt() |> ceil()}

  defp update_bounds("^", [{min1, max1}, {min2, max2}]) do
    if min1 <= 0 and max1 >= 0 and min2 < 0 do
      :unknown
    else
      base = if max2 < 0, do: min(abs(min1), abs(min2)), else: max(abs(min1), abs(max1))
      extent = :math.pow(base, max2)
      {min(floor(-extent), -1), max(ceil(extent), 1)}
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

  defp update_bounds("round", [{min, max}, {precision_min, _precision_max}]) do
    if precision_min < 0 do
      round_to = Cloak.Math.int_pow(10, -precision_min)
      {(div(min, round_to) - 1) * round_to, (div(max, round_to) + 1) * round_to}
    else
      {min, max}
    end
  end

  defp update_bounds("trunc", [{min, max}, {precision_min, precision_max}]) do
    for bound <- [min, max], precision <- [precision_min, precision_max] do
      if precision >= 0 do
        bound
      else
        trunc_to = Cloak.Math.int_pow(10, -precision)
        div(bound, trunc_to) * trunc_to
      end
    end
    |> Enum.min_max()
  end

  defp update_bounds(fun, [bounds]) when fun in ["floor", "ceil", "round", "trunc"], do: bounds
  defp update_bounds(_, _), do: :unknown

  defp floor(number), do: number |> :math.floor() |> round()
  defp ceil(number), do: number |> :math.ceil() |> round()

  # -------------------------------------------------------------------
  # Safety analysis
  # -------------------------------------------------------------------

  defp check_functions(expression) do
    update_in(
      expression,
      [Query.Lenses.all_expressions() |> Lens.filter(&(&1.function in Map.keys(@unsafe_names)))],
      fn expression ->
        if within_bounds?(expression.type, expression.bounds) do
          %{expression | function: Map.fetch!(@unsafe_names, expression.function)}
        else
          expression
        end
      end
    )
  end

  defp check_division(expression) do
    update_in(
      expression,
      [Query.Lenses.all_expressions() |> Lens.filter(&(&1.function in @divisions))],
      &do_check_division/1
    )
  end

  defp do_check_division(expression = %Expression{function: f, function_args: [%{bounds: :unknown}, _]})
       when f in @divisions,
       do: expression

  defp do_check_division(expression = %Expression{function: f, function_args: [_, %{bounds: :unknown}]})
       when f in @divisions,
       do: expression

  defp do_check_division(expression = %Expression{function: "%", function_args: [_, divisor], bounds: bounds}) do
    cond do
      !within_bounds?(:integer, bounds) -> expression
      spans_zero?(divisor.bounds) -> %{expression | function: "checked_mod"}
      true -> %{expression | function: "unsafe_mod"}
    end
  end

  defp do_check_division(expression = %Expression{function: "/", function_args: [dividend, divisor]}) do
    case {spans_zero?(divisor.bounds), div_epsilon(dividend)} do
      {false, _} ->
        %{expression | function: "unsafe_div"}

      {_, {:ok, epsilon}} ->
        %{expression | function: "checked_div", function_args: [dividend, divisor, Expression.constant(:real, epsilon)]}

      _ ->
        expression
    end
  end

  defp check_pow(expression) do
    update_in(
      expression,
      [Query.Lenses.all_expressions() |> Lens.filter(&(&1.function == "^")) |> Lens.filter(&(&1.bounds != :unknown))],
      fn expression = %Expression{function: "^", type: type, bounds: bounds, function_args: [base, _]} ->
        {base_min, _} = base.bounds

        if within_bounds?(type, bounds) and base_min < 0 do
          %{expression | function: "checked_pow"}
        else
          expression
        end
      end
    )
  end

  defp spans_zero?({min, max}), do: min < 1 && max > -1

  defp div_epsilon(%{bounds: {min, max}}) do
    magnitude = max(abs(min), abs(max))
    if magnitude < @large_float_number, do: {:ok, magnitude / @large_float_number}, else: :error
  end

  defp within_bounds?(:integer, {min, max}), do: min > -@max_int && max < @max_int
  defp within_bounds?(:real, {min, max}), do: min > -@large_float_number && max < @large_float_number
  defp within_bounds?(_, _), do: false
end
