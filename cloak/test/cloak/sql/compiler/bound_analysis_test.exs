defmodule Cloak.Sql.Compiler.BoundAnalysis.Test do
  use ExUnit.Case
  use ExUnitProperties

  alias Cloak.Sql.Compiler.BoundAnalysis
  alias Cloak.Sql.Expression
  alias Cloak.DataSource.Table

  defmacrop assert_unknown_or_within_bounds(expression, values, function) do
    quote bind_quoted: [expression: expression, values: values, function: function] do
      case BoundAnalysis.set_bounds(expression).bounds do
        :unknown ->
          true

        {min, max} ->
          result = apply(function, values)

          assert result <= max && result >= min,
                 "Bounds calculated: #{inspect({min, max})}. " <>
                   "Result of applying #{inspect(function)} to #{inspect(values)} is #{result}."
      end
    end
  end

  describe ".analyze_query" do
    test "sets bounds for each expression in the query" do
      assert {11, 21} = hd(compile!("SELECT 1 + column FROM table").columns).bounds
    end

    test "propagates bounds from subqueries" do
      compiled = compile!("SELECT foo FROM (SELECT 1 + column AS foo FROM table) bar")
      assert {11, 21} = hd(compiled.columns).bounds
    end

    defp compile!(query) do
      Cloak.Test.QueryHelpers.compile!(query, data_source())
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

  describe ".set_bounds" do
    test "integer constants" do
      assert {2, 2} = BoundAnalysis.set_bounds(Expression.constant(:integer, 2)).bounds
    end

    test "real constants" do
      assert {2, 2} = BoundAnalysis.set_bounds(Expression.constant(:real, 2)).bounds
    end

    test "small real constants" do
      assert {0, 1} = BoundAnalysis.set_bounds(Expression.constant(:real, 0.01)).bounds
    end

    test "null numeric constants" do
      assert :unknown = BoundAnalysis.set_bounds(Expression.constant(:real, nil)).bounds
    end

    test "other constants" do
      assert :unknown = BoundAnalysis.set_bounds(Expression.constant(:text, "Some text")).bounds
    end

    test "columns with bounds set are ignored" do
      assert BoundAnalysis.set_bounds(column_in_bounds({10, 20})) == column_in_bounds({10, 20})
    end

    test "[temporary] columns with no bounds are set to [10 - 20]" do
      assert {10, 20} = BoundAnalysis.set_bounds(column_in_bounds(:unknown)).bounds
    end

    test "sqrt bounds are tight for positive input bounds" do
      assert {10, 20} = BoundAnalysis.set_bounds(function_expression("sqrt", [column_in_bounds({100, 400})])).bounds
    end

    test "cast from integer to real" do
      expression =
        BoundAnalysis.set_bounds(function_expression({:cast, :real}, [column_in_bounds({100, 200}, :integer)]))

      assert {100, 200} = expression.bounds
    end

    test "cast from real to integer" do
      expression =
        BoundAnalysis.set_bounds(function_expression({:cast, :integer}, [column_in_bounds({100, 200}, :real)]))

      assert {100, 200} = expression.bounds
    end

    test "cast from boolean to integer" do
      expression =
        BoundAnalysis.set_bounds(function_expression({:cast, :integer}, [column_in_bounds(:unknown, :boolean)]))

      assert {0, 1} = expression.bounds
    end

    test "cast from boolean to real" do
      expression = BoundAnalysis.set_bounds(function_expression({:cast, :real}, [column_in_bounds(:unknown, :boolean)]))

      assert {0, 1} = expression.bounds
    end

    test "other cast" do
      expression =
        BoundAnalysis.set_bounds(function_expression({:cast, :integer}, [column_in_bounds({100, 200}, :timestamp)]))

      assert :unknown = expression.bounds
    end

    property "bounds can be computed for simplest arguments to function" do
      check all {name, function} <- function() do
        arity = Function.info(function) |> Keyword.fetch!(:arity)
        args = 1..arity |> Enum.map(fn _ -> column_in_bounds({2, 2}) end)
        expression = function_expression(name, args)
        assert BoundAnalysis.set_bounds(expression).bounds != :unknown
      end
    end

    property "expression result is within computed bounds" do
      check all {name, function} <- function(),
                bounds <- list_of(bounds(), length: Function.info(function) |> Keyword.fetch!(:arity)),
                values <- values(bounds),
                max_runs: 500 do
        expression = function_expression(name, Enum.map(bounds, &column_in_bounds/1))
        assert_unknown_or_within_bounds(expression, values, function)
      end
    end

    test "generated example 1" do
      bounds = [{-147, -20}, {-216, -175}]
      values = [-26.553144616558242, -216.0]
      expression = function_expression("^", Enum.map(bounds, &column_in_bounds/1))
      assert_unknown_or_within_bounds(expression, values, &:math.pow/2)
    end

    test "generated example 2" do
      bounds = [{-134, -12}, {-310, -162}]
      values = [-73.22209099941092, -172.57340641899032]
      expression = function_expression("^", Enum.map(bounds, &column_in_bounds/1))
      assert_unknown_or_within_bounds(expression, values, &safe_pow/2)
    end

    test "generated example 3" do
      bounds = [{-2, 7}, {-8, 6}]
      values = [7.0, -1.0]
      expression = function_expression("round", Enum.map(bounds, &column_in_bounds/1))
      assert_unknown_or_within_bounds(expression, values, &safe_round/2)
    end
  end

  describe ".analyze_safety" do
    @max_int 9_223_372_036_854_775_807

    test "/ with safe argument bounds results in an unsafe_div" do
      dividend = column_in_bounds({10, 20})
      divisor = column_in_bounds({10, 20})

      assert %Expression{function: "unsafe_div", function_args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "/ with reasonable argument bounds results in a checked_div" do
      dividend = column_in_bounds({10, 20})
      divisor = column_in_bounds({-10, 10})

      assert %Expression{function: "checked_div", function_args: [^dividend, ^divisor, %Expression{value: epsilon}]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))

      assert epsilon < 1
      assert 20 / epsilon < 1.0e101
    end

    test "/ with unreasonable argument bounds results in a /" do
      dividend = column_in_bounds({round(-1.0e200), round(1.0e200)})
      divisor = column_in_bounds({-1, 1})

      assert %Expression{function: "/", function_args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "/ with unknown bounds on dividend" do
      dividend = column_in_bounds(:unknown)
      divisor = column_in_bounds({10, 20})

      assert %Expression{function: "/", function_args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "/ with unknown bounds on divisor" do
      dividend = column_in_bounds({10, 20})
      divisor = column_in_bounds(:unknown)

      assert %Expression{function: "/", function_args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "% with divisor spanning 0" do
      expression =
        %Expression{function: "%", function_args: [column_in_bounds({10, 20}), column_in_bounds({-10, 10})]}
        |> set_bounds({0, 100})

      assert BoundAnalysis.analyze_safety(expression) == %{expression | function: "checked_mod"}
    end

    test "% with divisor not spanning 0" do
      expression =
        %Expression{function: "%", function_args: [column_in_bounds({10, 20}), column_in_bounds({-100, -10})]}
        |> set_bounds({0, 100})

      assert BoundAnalysis.analyze_safety(expression) == %{expression | function: "unsafe_mod"}
    end

    test "% with too large output bounds" do
      expression =
        %Expression{function: "%", function_args: [column_in_bounds({10, 20}), column_in_bounds({-100, -10})]}
        |> set_bounds({0, @max_int + 1})

      assert BoundAnalysis.analyze_safety(expression) == expression
    end

    test "integer expression with result within 64bit unsigned bounds" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a]) |> set_bounds({100, 200})
      assert %Expression{function: "unsafe_add", function_args: [^a, ^a]} = BoundAnalysis.analyze_safety(expression)
    end

    test "integer expression with result outside of 64bit unsigned bounds" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a]) |> set_bounds({0, @max_int + 1})
      assert ^expression = BoundAnalysis.analyze_safety(expression)
    end

    test "real expression with magnitude of result smaller than 1.0e100" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a], :real) |> set_bounds({0, 1.0e50})
      assert %Expression{function: "unsafe_add", function_args: [^a, ^a]} = BoundAnalysis.analyze_safety(expression)
    end

    test "real expression with magnitude of result larger than 1.0e100" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a], :real) |> set_bounds({0, 1.0e101})
      assert ^expression = BoundAnalysis.analyze_safety(expression)
    end

    test "^ that could result in a complex number" do
      a = column_in_bounds({-20, 20})
      b = column_in_bounds({-1, 1})
      expression = function("^", [a, b], :real) |> set_bounds({-20, 20})
      assert ^expression = BoundAnalysis.analyze_safety(expression)
    end
  end

  describe ".cast_arguments" do
    test "casts arguments of math functions" do
      a = column_in_bounds(:unknown, :integer)
      b = column_in_bounds(:unknown, :integer)
      expression = function("+", [a, b], :real)

      assert %Expression{
               function: "+",
               function_args: [
                 %Expression{function: {:cast, :integer}, function_args: [^a]},
                 %Expression{function: {:cast, :integer}, function_args: [^b]}
               ]
             } = BoundAnalysis.cast_arguments(expression)
    end

    test "ignores reals" do
      a = column_in_bounds(:unknown, :integer)
      b = column_in_bounds(:unknown, :real)
      expression = function("+", [a, b], :real)

      assert %Expression{
               function: "+",
               function_args: [%Expression{function: {:cast, :integer}, function_args: [^a]}, ^b]
             } = BoundAnalysis.cast_arguments(expression)
    end

    test "ignores arguments of non-math functions" do
      a = column_in_bounds(:unknown, :integer)
      expression = function({:cast, :string}, [a], :string)

      assert ^expression = BoundAnalysis.cast_arguments(expression)
    end

    test "ignores constants" do
      a = column_in_bounds({10, 20})
      b = Expression.constant(:integer, 20)
      expression = function("+", [a, b])

      assert %Expression{function: "+", function_args: [_, ^b]} = BoundAnalysis.cast_arguments(expression)
    end
  end

  defp function(name, args, type \\ :integer) do
    Expression.function(name, args, type)
  end

  defp column_in_bounds(bounds, type \\ :real) do
    Expression.column(%{name: "column", type: type}, table()) |> set_bounds(bounds)
  end

  defp set_bounds(expression, bounds) do
    put_in(expression, [Lens.key(:bounds)], bounds)
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
      constant({"round", &safe_round/2}),
      constant({"trunc", &Kernel.trunc/1}),
      constant({"trunc", &safe_trunc/2}),
      constant({"sqrt", &safe_sqrt/1}),
      constant({"%", &round_rem/2})
    ])
  end

  defp safe_round(number, precision), do: Cloak.Math.round(number, precision |> round() |> min(15) |> max(-100))

  defp safe_trunc(number, precision), do: Cloak.Math.trunc(number, precision |> round() |> min(15) |> max(-100))

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
