defmodule Cloak.Sql.Compiler.BoundAnalysis.Test do
  use ExUnit.Case
  use ExUnitProperties

  alias Cloak.Sql.Compiler.BoundAnalysis
  alias Cloak.Sql.Expression
  alias Cloak.DataSource.Table

  describe ".analyze_query" do
    test "sets bounds for each expression in the query" do
      assert {11, 21} = hd(compile!("SELECT 1 + column, median(column) FROM table GROUP BY column").columns).bounds
    end

    test "propagates bounds from subqueries"

    defp compile!(query) do
      query
      |> Cloak.Test.QueryHelpers.compile!(data_source())
      |> BoundAnalysis.analyze_query()
    end

    defp data_source() do
      %{
        name: "bound_analysis_data_source",
        driver: Cloak.DataSource.PostgreSQL,
        tables: %{
          table:
            Table.new("table", "uid",
              db_name: "table",
              columns: [
                Table.column("uid", :integer),
                Table.column("column", :integer)
              ]
            )
        }
      }
    end
  end

  describe ".analyze_expression" do
    test "integer constants" do
      assert {2, 2} = BoundAnalysis.analyze_expression(Expression.constant(:integer, 2)).bounds
    end

    test "real constants" do
      assert {2, 2} = BoundAnalysis.analyze_expression(Expression.constant(:real, 2)).bounds
    end

    test "other constants" do
      assert :unknown = BoundAnalysis.analyze_expression(Expression.constant(:text, "Some text")).bounds
    end

    test "columns with bounds set are ignored" do
      assert {10, 20} = BoundAnalysis.analyze_expression(column_in_bounds({10, 20})).bounds
    end

    test "[temporary] columns with no bounds are set to [10 - 20]" do
      assert {10, 20} = BoundAnalysis.analyze_expression(column_in_bounds(:unknown)).bounds
    end

    property "bounds can be computed for simplest arguments to function" do
      check all {name, function} <- function() do
        arity = Function.info(function) |> Keyword.fetch!(:arity)
        args = 1..arity |> Enum.map(fn _ -> column_in_bounds({2, 2}) end)
        expression = function_expression(name, args)
        assert BoundAnalysis.analyze_expression(expression).bounds != :unknown
      end
    end

    property "expression result is within computed bounds" do
      check all {name, function} <- function(),
                bounds <- list_of(bounds(), length: Function.info(function) |> Keyword.fetch!(:arity)),
                values <- values(bounds),
                max_runs: 500 do
        expression = function_expression(name, Enum.map(bounds, &column_in_bounds/1))
        assert unknown_or_within_bounds(expression, values, function)
      end
    end

    test "generated example 1" do
      bounds = [{-147, -20}, {-216, -175}]
      values = [-26.553144616558242, -216.0]
      expression = function_expression("^", Enum.map(bounds, &column_in_bounds/1))
      assert unknown_or_within_bounds(expression, values, &:math.pow/2)
    end
  end

  defp unknown_or_within_bounds(expression, values, function) do
    case BoundAnalysis.analyze_expression(expression).bounds do
      :unknown ->
        true

      {min, max} ->
        result = apply(function, values)
        result <= max && result >= min
    end
  end

  defp column_in_bounds(bounds) do
    %{Expression.column(%{name: "column", type: "real"}, table()) | bounds: bounds}
  end

  defp function_expression(function_name, args) do
    Expression.function(function_name, args, :real)
  end

  defp function() do
    one_of([
      constant({"+", &Kernel.+/2}),
      constant({"-", &Kernel.-/2}),
      constant({"*", &Kernel.*/2}),
      constant({"/", &Kernel.//2}),
      constant({"^", &safe_pow/2}),
      constant({"abs", &Kernel.abs/1}),
      constant({"floor", &:math.floor/1}),
      constant({"ceil", &:math.ceil/1}),
      constant({"round", &Kernel.round/1}),
      constant({"trunc", &Kernel.trunc/1}),
      constant({"sqrt", &safe_sqrt/1}),
      constant({"%", &round_rem/2})
    ])
  end

  defp safe_pow(a, b), do: if(a < 0, do: :math.pow(a, :math.floor(b)), else: :math.pow(a, b))

  defp safe_sqrt(number), do: if(number < 0, do: 0, else: :math.sqrt(number))

  defp round_rem(a, b), do: if(round(b) == 0, do: 0, else: rem(round(a), round(b)))

  defp bounds() do
    gen all a <- integer(), b <- integer() do
      {min(a, b), max(a, b)}
    end
  end

  defp values(bounds) do
    Enum.reduce(bounds, constant([]), fn bound, acc_generator ->
      bind(acc_generator, fn acc_value ->
        value(bound)
        |> map(fn bound_value -> [bound_value | acc_value] end)
      end)
    end)
    |> map(&Enum.reverse/1)
  end

  defp value({min, max}), do: float(min: min, max: max)

  defp table(), do: %{keys: %{}}
end
