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
      |> update_in([columns_lens()], &set_leaf_bounds(&1, subquery))
      |> update_in([Query.Lenses.query_expressions()], &set_bounds/1)
      |> update_in([Query.Lenses.query_expressions()], &analyze_safety/1)
    end)
  end

  @doc "Takes a query and clamps all bounded columns."
  @spec clamp_columns_to_bounds(Query.t()) :: Query.t()
  def clamp_columns_to_bounds(query) do
    query
    |> Helpers.apply_bottom_up(&expand_tables_with_bounded_columns/1)
    |> Helpers.apply_bottom_up(fn subquery ->
      bounded_database_columns_lens() |> Lens.map(subquery, &clamp_values/1)
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
  # Internal functions
  # -------------------------------------------------------------------

  defp used_columns_from_table(query, table_name) do
    columns_lens()
    |> Lens.filter(&(&1.table.name == table_name))
    |> Lens.to_list(query)
    |> Enum.map(&Expression.unalias/1)
    |> Enum.uniq_by(& &1.name)
  end

  defp expand_regular_table(query, table_alias) do
    table = Query.resolve_table(query, table_alias)
    columns = query |> used_columns_from_table(table_alias) |> Enum.map(&Map.put(&1, :table, table))

    ast = Cloak.Sql.Compiler.make_select_query(query.data_source, table, columns)
    {:subquery, %{ast: ast, alias: table_alias}}
  end

  defp expand_virtual_table(query, subquery) do
    table = Query.resolve_table(query, subquery.alias)
    columns = query |> used_columns_from_table(subquery.alias) |> Enum.map(&Map.put(&1, :table, table))

    ast =
      Cloak.Sql.Compiler.make_select_query(query.data_source, table, columns)
      |> Map.put(:from, {:subquery, %{ast: subquery.ast, alias: table.name}})

    %{ast: ast, alias: subquery.alias}
  end

  deflensp query_tables_lens() do
    Lens.multiple([
      columns_lens() |> Lens.key(:table),
      Lens.key(:selected_tables) |> Lens.all(),
      Lens.key(:table_aliases) |> Lens.map_values()
    ])
  end

  defp expand_tables_with_bounded_columns(query) do
    expanded_tables =
      bounded_database_columns_lens()
      |> Lens.key(:table)
      |> Lens.key(:name)
      |> Lens.to_list(query)
      |> Expression.unique()

    query
    |> update_in(
      [Query.Lenses.direct_subqueries(analyst_tables?: false) |> Lens.filter(&(&1.alias in expanded_tables))],
      &expand_virtual_table(query, &1)
    )
    |> update_in(
      [Query.Lenses.leaf_tables() |> Lens.filter(&(&1 in expanded_tables))],
      &expand_regular_table(query, &1)
    )
    |> put_in([query_tables_lens() |> Lens.filter(&(&1.name in expanded_tables)) |> Lens.key(:type)], :subquery)
  end

  # -------------------------------------------------------------------
  # Bound computation
  # -------------------------------------------------------------------

  defp set_leaf_bounds(expression, query) do
    case Query.resolve_subquery_column(expression, query) do
      :database_column ->
        %{expression | bounds: Bounds.bounds(query.data_source, expression.table.initial_name, expression)}

      {column, _subquery} ->
        %{expression | bounds: column.bounds}
    end
  end

  deflensp columns_lens(), do: Query.Lenses.query_expressions() |> Lens.filter(&Expression.column?/1)

  deflensp bounded_database_columns_lens() do
    columns_lens() |> Lens.filter(&(&1.bounds != :unknown)) |> Lens.filter(&(&1.table.type in [:regular, :virtual]))
  end

  defp do_set_bounds(expression = %Expression{kind: :constant, value: value}) when value in [:*, nil],
    do: expression

  defp do_set_bounds(expression = %Expression{kind: :constant, type: type, value: value})
       when type in [:integer, :real],
       do: %{expression | bounds: {floor(value), ceil(value)}}

  defp do_set_bounds(expression = %Expression{kind: :constant, type: type, value: value})
       when type in [:date, :datetime],
       do: %{expression | bounds: {value.year, value.year}}

  defp do_set_bounds(expression = %Expression{kind: :constant, type: :interval, value: value}) do
    years = Timex.Duration.to_days(value) / 365.25
    %{expression | bounds: {floor(years), ceil(years)}}
  end

  defp do_set_bounds(expression = %Expression{kind: :column}),
    do: expression

  defp do_set_bounds(expression = %Expression{kind: :function, args: [:*]}),
    do: expression

  defp do_set_bounds(expression = %Expression{kind: :function, args: [{:distinct, _}]}),
    do: expression

  defp do_set_bounds(
         expression = %Expression{kind: :function, name: {:cast, to}, args: [%{bounds: bounds, type: from}]}
       )
       when from in [:real, :integer, :date, :datetime] and to in [:real, :integer, :date, :datetime],
       do: %{expression | bounds: bounds}

  defp do_set_bounds(expression = %Expression{kind: :function, name: {:cast, to}, args: [%{type: :boolean}]})
       when to in [:real, :integer],
       do: %{expression | bounds: {0, 1}}

  defp do_set_bounds(expression = %Expression{kind: :function, name: name, args: args}),
    do: %{expression | bounds: update_bounds(name, Enum.map(args, & &1.bounds))}

  defp do_set_bounds(expression), do: %{expression | bounds: :unknown}

  defp update_bounds("year", [bounds]), do: bounds
  defp update_bounds("quarter", [_bounds]), do: {1, 4}
  defp update_bounds("month", [_bounds]), do: {1, 12}
  defp update_bounds("day", [_bounds]), do: {1, 31}
  defp update_bounds("hour", [_bounds]), do: {0, 23}
  defp update_bounds("minute", [_bounds]), do: {0, 59}
  defp update_bounds("second", [_bounds]), do: {0, 60}
  defp update_bounds("weekday", [_bounds]), do: {1, 7}

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

  defp update_bounds(fun, [bounds]) when fun in ~w(floor ceil round trunc min max avg), do: bounds
  defp update_bounds(_, _), do: :unknown

  defp clamp_values(%Expression{type: type, bounds: {min, max}} = column) when type in [:integer, :real] do
    min_constant = Expression.constant(type, min)
    max_constant = Expression.constant(type, max)

    Expression.function(
      "case",
      [
        Expression.function("<", [column, min_constant], :boolean),
        min_constant,
        Expression.function(">", [column, max_constant], :boolean),
        max_constant,
        column
      ],
      type
    )
    |> Map.put(:bounds, {min, max})
    |> Map.put(:synthetic?, true)
  end

  defp clamp_values(%Expression{type: type, bounds: {min, max}} = column) when type in [:date, :datetime] do
    column_year = Expression.function("year", [column], :integer)

    Expression.function(
      "case",
      [
        Expression.function("<", [column_year, Expression.constant(:integer, min)], :boolean),
        Expression.constant(type, min_year_to_value(type, min)),
        Expression.function(">", [column_year, Expression.constant(:integer, max)], :boolean),
        Expression.constant(type, max_year_to_value(type, max)),
        column
      ],
      type
    )
    |> Map.put(:bounds, {min, max})
    |> Map.put(:synthetic?, true)
  end

  defp min_year_to_value(:date, year) do
    {:ok, value} = Date.new(year, 1, 1)
    value
  end

  defp min_year_to_value(:datetime, year) do
    {:ok, value} = NaiveDateTime.new(year, 1, 1, 0, 0, 0)
    value
  end

  defp max_year_to_value(:date, year) do
    {:ok, value} = Date.new(year, 12, 31)
    value
  end

  defp max_year_to_value(:datetime, year) do
    {:ok, value} = NaiveDateTime.new(year, 12, 31, 23, 59, 59)
    value
  end

  # -------------------------------------------------------------------
  # Safety analysis
  # -------------------------------------------------------------------

  defp check_functions(expression) do
    update_in(
      expression,
      [
        Query.Lenses.all_expressions()
        |> Lens.filter(&Expression.function?/1)
        |> Lens.filter(&(&1.name in Map.keys(@unsafe_names)))
      ],
      fn expression ->
        if within_bounds?(expression.type, expression.bounds) do
          %{expression | name: Map.fetch!(@unsafe_names, expression.name)}
        else
          expression
        end
      end
    )
  end

  defp check_division(expression) do
    update_in(
      expression,
      [
        Query.Lenses.all_expressions()
        |> Lens.filter(&Expression.function?/1)
        |> Lens.filter(&(&1.name in @divisions))
      ],
      &do_check_division/1
    )
  end

  defp do_check_division(expression = %Expression{kind: :function, name: name, args: [%{bounds: :unknown}, _]})
       when name in @divisions,
       do: expression

  defp do_check_division(expression = %Expression{kind: :function, name: name, args: [_, %{bounds: :unknown}]})
       when name in @divisions,
       do: expression

  defp do_check_division(expression = %Expression{kind: :function, name: "%", args: [_, divisor], bounds: bounds}) do
    cond do
      !within_bounds?(:integer, bounds) -> expression
      spans_zero?(divisor.bounds) -> %{expression | name: "checked_mod"}
      true -> %{expression | name: "unsafe_mod"}
    end
  end

  defp do_check_division(expression = %Expression{kind: :function, name: "/", args: [dividend, divisor]}) do
    case {spans_zero?(divisor.bounds), div_epsilon(dividend)} do
      {false, _} ->
        %{expression | name: "unsafe_div"}

      {_, {:ok, epsilon}} ->
        %{expression | name: "checked_div", args: [dividend, divisor, Expression.constant(:real, epsilon)]}

      _ ->
        expression
    end
  end

  defp check_pow(expression) do
    update_in(
      expression,
      [
        Query.Lenses.all_expressions()
        |> Lens.filter(&Expression.function?/1)
        |> Lens.filter(&(&1.name == "^"))
        |> Lens.filter(&(&1.bounds != :unknown))
      ],
      fn expression = %Expression{kind: :function, name: "^", type: type, bounds: bounds, args: [base, _]} ->
        {base_min, _} = base.bounds

        if within_bounds?(type, bounds) and base_min < 0 do
          %{expression | name: "checked_pow"}
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

  defp within_bounds?(:integer, {min, max}), do: min > -@max_int and max < @max_int
  defp within_bounds?(:real, {min, max}), do: min > -@large_float_number and max < @large_float_number

  defp within_bounds?(type, {min, max}) when type in [:date, :datetime],
    do: min >= Cloak.Time.year_lower_bound() and max <= Cloak.Time.year_upper_bound()

  defp within_bounds?(_, _), do: false
end
