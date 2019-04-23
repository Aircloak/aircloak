defmodule Cloak.Sql.Compiler.StaticAnalysis.Test do
  alias Test.StaticAnalysisReport
  alias Cloak.Sql.Compiler.StaticAnalysis
  alias Cloak.Sql.{Compiler, Parser, Expression}

  use ExUnit.Case

  @positive_infinity 1.0e305

  @problems [
    # equation, argument bounds, output bounds, result
    {"1 / (x1 - 1)", %{"x1" => {-100, 100}}, {-200, 200}, :divide_by_zero},
    {"1 / (x1 - 1)", %{"x1" => {100, 200}}, {-200, 200}, :ok},
    {"1 / x1", %{"x1" => {-100, 100}}, {-200, 200}, :divide_by_zero},
    {"1 / x1", %{"x1" => {100, 200}}, {-200, 200}, :ok},
    {"abs(100 * (x2 * x2 - 0.01 * x1 * x1 + 1) + 0.01 * (x1 + 10) * (x1 + 10))", %{"x1" => {-15, -5}, "x2" => {-3, 3}},
     {0, @positive_infinity}, :ok},
    {"abs(100 * (x2 * x2 - 0.01 * x1 * x1 + 1) + 0.01 * (x1 + 10) * (x1 + 10))", %{"x1" => {-15, -5}, "x2" => {-3, 3}},
     {1, @positive_infinity}, :overflow}
  ]

  describe ".to_function" do
    test "converting a simple function" do
      compiled = compile!("SELECT COUNT(*) FROM table WHERE x1 * x2 = 0")

      {function, _input_bounds} =
        StaticAnalysis.to_function(condition_expression(compiled), bound_builder(%{"x1" => nil, "x2" => nil}))

      assert function.([13, 17]) == 13 * 17
    end
  end

  Enum.each(@problems, fn problem = {equation, input_bounds, output_bounds, result} ->
    test "analyzing #{inspect(problem)}" do
      compiled = compile!("SELECT COUNT(*) FROM table WHERE #{unquote(equation)} = 0")
      input_bounds = unquote(Macro.escape(input_bounds))
      bound_builder = bound_builder(input_bounds)
      {function, input_bounds} = StaticAnalysis.to_function(condition_expression(compiled), bound_builder)

      for method <- methods() do
        StaticAnalysisReport.report(
          unquote(equation),
          unquote(Macro.escape(input_bounds)),
          unquote(output_bounds),
          unquote(result) == :ok,
          method,
          StaticAnalysis.safe?(method, function, input_bounds, unquote(output_bounds)),
          passes_validation?(compiled)
        )
      end
    end
  end)

  defp methods() do
    [StaticAnalysis.SimulatedAnnealing]
  end

  defp bound_builder(bounds) do
    fn %Expression{name: name} ->
      bounds[name]
    end
  end

  defp passes_validation?(query) do
    query
    |> put_in([Lens.key(:type)], :anonymized)
    |> Compiler.Validation.verify_standard_restrictions()
    |> Compiler.Validation.verify_anonymization_restrictions()
    |> Compiler.TypeChecker.validate_allowed_usage_of_math_and_functions()

    true
  rescue
    Cloak.Sql.CompilationError -> false
  end

  defp condition_expression(query) do
    {:comparison, expression, :=, _} = query.where
    expression
  end

  defp compile!(query) do
    parsed = Parser.parse!(query)
    Compiler.compile_direct!(parsed, 1, data_source())
  end

  defp data_source() do
    alias Cloak.DataSource.Table

    %{
      name: "static_analysis_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer)
              | 1..10 |> Enum.map(&Table.column("x#{&1}", :integer))
            ]
          )
      }
    }
  end
end
