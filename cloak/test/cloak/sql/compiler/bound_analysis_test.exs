defmodule Cloak.Sql.Compiler.BoundAnalysis.Test do
  use ExUnit.Case, async: false
  use ExUnitProperties

  alias Cloak.Sql.Compiler.BoundAnalysis
  alias Cloak.Sql.Expression

  import Cloak.Test.QueryHelpers

  setup_all do
    :ok = Cloak.Test.DB.create_table("bounds_analysis", "col INTEGER, d DATE, oob_date DATE")

    :ok =
      insert_rows(_user_ids = 1..5, "bounds_analysis", ["col", "d", "oob_date"], [30, ~D[2000-05-03], ~D[9999-11-01]])

    :ok =
      insert_rows(_user_ids = 6..10, "bounds_analysis", ["col", "d", "oob_date"], [70, ~D[2010-02-12], ~D[1000-02-10]])

    :ok =
      Cloak.Test.DB.create_table("bounds_analysis_virtual", nil,
        skip_db_create: true,
        query: "select user_id, col * 2 as col from cloak_test.bounds_analysis"
      )

    data_sources = Cloak.DataSource.all()

    analysis_data_source =
      default_data_source()
      |> Map.put(:name, "analysis")
      |> Map.put(:bound_computation_enabled, true)
      |> Map.put(:statistics_anonymization, false)

    Cloak.DataSource.replace_all_data_source_configs([analysis_data_source])

    Cloak.Air.register_air("some air")

    {:ok, _} =
      Cloak.Test.AnalystTableHelpers.create_or_update(
        1,
        "bounds_analysis_analyst",
        "select user_id as uid, col * 2 as col from bounds_analysis ba where ba.col between 0 and 100",
        analysis_data_source
      )

    {:ok, _} =
      Cloak.Test.AnalystTableHelpers.create_or_update(
        1,
        "bounds_analysis_analyst_recursive",
        "select uid, baa.col * 2 as col from bounds_analysis_analyst baa group by 1, 2",
        analysis_data_source
      )

    on_exit(fn ->
      Cloak.Air.unregister_air()
      Cloak.DataSource.replace_all_data_source_configs(data_sources)
    end)

    {:ok, analysis_data_source}
  end

  test "numeric columns with known bounds are restricted during offload", analysis_data_source do
    offloaded_query =
      "SELECT COUNT(*) FROM bounds_analysis t WHERE col = 10"
      |> compile!(analysis_data_source)
      |> Cloak.Sql.Query.resolve_db_columns()
      |> Cloak.DataSource.SqlBuilder.build()

    assert offloaded_query =~
             ~s[CASE WHEN ("bounds_analysis"."col" < 2) THEN 2] <>
               ~s[ WHEN ("bounds_analysis"."col" > 1000) THEN 1000 ELSE "bounds_analysis"."col" END]
  end

  test "date columns with known bounds are restricted during offload", analysis_data_source do
    offloaded_query =
      "SELECT COUNT(*) FROM bounds_analysis t WHERE d = '2000-05-03'"
      |> compile!(analysis_data_source)
      |> Cloak.Sql.Query.resolve_db_columns()
      |> Cloak.DataSource.SqlBuilder.build()

    assert offloaded_query =~
             ~s[CASE WHEN (EXTRACT(year FROM "bounds_analysis"."d") < 1975) THEN date '1975-01-01'] <>
               ~s[ WHEN (EXTRACT(year FROM "bounds_analysis"."d") > 2125) THEN date '2125-12-31'] <>
               ~s[ ELSE "bounds_analysis"."d" END]
  end

  test "multiple columns with known bounds are restricted during offload", analysis_data_source do
    offloaded_query =
      "SELECT year(d) FROM bounds_analysis t WHERE t.col BETWEEN 0 AND 100 GROUP BY 1"
      |> compile!(analysis_data_source)
      |> Cloak.Sql.Query.resolve_db_columns()
      |> Cloak.DataSource.SqlBuilder.build()

    # assert known columns are restricted in the bottom subquery

    assert offloaded_query =~
             ~s[CASE WHEN (EXTRACT(year FROM "bounds_analysis"."d") < 1975) THEN date '1975-01-01'] <>
               ~s[ WHEN (EXTRACT(year FROM "bounds_analysis"."d") > 2125) THEN date '2125-12-31'] <>
               ~s[ ELSE "bounds_analysis"."d" END]

    assert offloaded_query =~
             ~s[CASE WHEN ("bounds_analysis"."col" < 2) THEN 2] <>
               ~s[ WHEN ("bounds_analysis"."col" > 1000) THEN 1000 ELSE "bounds_analysis"."col" END]

    # assert columns are restricted only once
    assert offloaded_query =~ ~s[("t"."col" >= 0) AND ("t"."col" < 100)]
  end

  test "bounded columns in virtual tables are restricted during offload", analysis_data_source do
    offloaded_query =
      "SELECT COUNT(*) FROM bounds_analysis_virtual WHERE col = 10"
      |> compile!(analysis_data_source)
      |> Cloak.Sql.Query.resolve_db_columns()
      |> Cloak.DataSource.SqlBuilder.build()

    assert offloaded_query =~
             ~s[CASE WHEN ("bounds_analysis_virtual"."col" < 5) THEN 5] <>
               ~s[ WHEN ("bounds_analysis_virtual"."col" > 2000) THEN 2000 ELSE "bounds_analysis_virtual"."col" END]

    refute offloaded_query =~ "PG_TEMP.AC_MUL"
  end

  describe ".analyze_query" do
    test "sets bounds for each expression in the query", analysis_data_source do
      column = "SELECT 1 + col FROM bounds_analysis" |> compile!(analysis_data_source) |> first_selected_column()
      assert {3, 1001} = column.bounds
    end

    test "sets bounds aliased table", analysis_data_source do
      column = "SELECT 1 + col FROM bounds_analysis t1" |> compile!(analysis_data_source) |> first_selected_column()
      assert {3, 1001} = column.bounds
    end

    test "propagates bounds from subqueries", analysis_data_source do
      column =
        "SELECT foo FROM (SELECT 1 + col AS foo FROM bounds_analysis) bar"
        |> compile!(analysis_data_source)
        |> first_selected_column()

      assert {3, 1001} = column.bounds
    end

    test "sets bounds for virtual tables", analysis_data_source do
      column = "SELECT col FROM bounds_analysis_virtual" |> compile!(analysis_data_source) |> first_selected_column()
      assert {5, 2000} = column.bounds
    end

    test "sets bounds for analyst tables", analysis_data_source do
      column =
        "SELECT col FROM bounds_analysis_analyst"
        |> compile!(analysis_data_source, analyst: 1)
        |> first_selected_column()

      assert {4, 2000} = column.bounds
    end

    test "sets bounds for recursive analyst tables", analysis_data_source do
      column =
        "SELECT col FROM bounds_analysis_analyst_recursive"
        |> compile!(analysis_data_source, analyst: 1)
        |> first_selected_column()

      assert {8, 4000} = column.bounds
    end

    test "extreme date bounds are clamped", analysis_data_source do
      column = "SELECT oob_date FROM bounds_analysis" |> compile!(analysis_data_source) |> first_selected_column()
      assert {1900, 9999} = column.bounds
    end

    defp first_selected_column(query) do
      {:subquery, uid_grouping_query} = query.from
      [_uid, first_group | _] = uid_grouping_query.ast.columns
      first_group
    end
  end

  describe ".set_bounds" do
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

    test "columns are ignored" do
      assert BoundAnalysis.set_bounds(column_in_bounds({10, 20})) == column_in_bounds({10, 20})
      assert BoundAnalysis.set_bounds(column_in_bounds(:unknown)) == column_in_bounds(:unknown)
    end

    test "sqrt bounds are tight for positive input bounds" do
      assert {10, 20} = BoundAnalysis.set_bounds(function("sqrt", [column_in_bounds({100, 400})], :real)).bounds
    end

    test "cast from integer to real" do
      expression = BoundAnalysis.set_bounds(function({:cast, :real}, [column_in_bounds({100, 200}, :integer)], :real))

      assert {100, 200} = expression.bounds
    end

    test "cast from real to integer" do
      expression = BoundAnalysis.set_bounds(function({:cast, :integer}, [column_in_bounds({100, 200}, :real)], :real))

      assert {100, 200} = expression.bounds
    end

    test "cast from boolean to integer" do
      expression = BoundAnalysis.set_bounds(function({:cast, :integer}, [column_in_bounds(:unknown, :boolean)], :real))

      assert {0, 1} = expression.bounds
    end

    test "cast from boolean to real" do
      expression = BoundAnalysis.set_bounds(function({:cast, :real}, [column_in_bounds(:unknown, :boolean)], :real))

      assert {0, 1} = expression.bounds
    end

    test "other cast" do
      expression =
        BoundAnalysis.set_bounds(function({:cast, :integer}, [column_in_bounds({100, 200}, :timestamp)], :real))

      assert :unknown = expression.bounds
    end

    for function <- ~w(avg min max) do
      test function do
        assert {123, 245} =
                 BoundAnalysis.set_bounds(function(unquote(function), [column_in_bounds({123, 245})], :real)).bounds
      end
    end

    property "bounds can be computed for simplest arguments to function" do
      check all({name, function} <- function()) do
        arity = Function.info(function) |> Keyword.fetch!(:arity)
        args = 1..arity |> Enum.map(fn _ -> column_in_bounds({2, 2}) end)
        expression = function(name, args, :real)
        assert BoundAnalysis.set_bounds(expression).bounds != :unknown
      end
    end

    property "expression result is within computed bounds" do
      check all(
              {name, function} <- function(),
              bounds <- list_of(bounds(), length: Function.info(function) |> Keyword.fetch!(:arity)),
              values <- values(bounds),
              max_runs: 500
            ) do
        expression = function(name, Enum.map(bounds, &column_in_bounds/1), :real)
        assert_unknown_or_within_bounds(expression, values, function)
      end
    end

    test "generated example 1" do
      bounds = [{-147, -20}, {-216, -175}]
      values = [-26.553144616558242, -216.0]
      expression = function("^", Enum.map(bounds, &column_in_bounds/1), :real)
      assert_unknown_or_within_bounds(expression, values, &:math.pow/2)
    end

    test "generated example 2" do
      bounds = [{-134, -12}, {-310, -162}]
      values = [-73.22209099941092, -172.57340641899032]
      expression = function("^", Enum.map(bounds, &column_in_bounds/1), :real)
      assert_unknown_or_within_bounds(expression, values, &safe_pow/2)
    end

    test "generated example 3" do
      bounds = [{-2, 7}, {-8, 6}]
      values = [7.0, -1.0]
      expression = function("round", Enum.map(bounds, &column_in_bounds/1), :real)
      assert_unknown_or_within_bounds(expression, values, &safe_round/2)
    end
  end

  describe ".analyze_safety" do
    @max_int 9_223_372_036_854_775_807

    test "/ with safe argument bounds results in an unsafe_div" do
      dividend = column_in_bounds({10, 20})
      divisor = column_in_bounds({10, 20})

      assert %Expression{name: "unsafe_div", args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "/ with reasonable argument bounds results in a checked_div" do
      dividend = column_in_bounds({10, 20})
      divisor = column_in_bounds({-10, 10})

      assert %Expression{name: "checked_div", args: [^dividend, ^divisor, %Expression{value: epsilon}]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))

      assert epsilon < 1
      assert 20 / epsilon < 1.0e101
    end

    test "/ with unreasonable argument bounds results in a /" do
      dividend = column_in_bounds({round(-1.0e200), round(1.0e200)})
      divisor = column_in_bounds({-1, 1})

      assert %Expression{name: "/", args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "/ with unknown bounds on dividend" do
      dividend = column_in_bounds(:unknown)
      divisor = column_in_bounds({10, 20})

      assert %Expression{name: "/", args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "/ with unknown bounds on divisor" do
      dividend = column_in_bounds({10, 20})
      divisor = column_in_bounds(:unknown)

      assert %Expression{name: "/", args: [^dividend, ^divisor]} =
               BoundAnalysis.analyze_safety(function("/", [dividend, divisor]))
    end

    test "% with divisor spanning 0" do
      expression =
        function("%", [column_in_bounds({10, 20}), column_in_bounds({-10, 10})])
        |> set_bounds({0, 100})

      assert BoundAnalysis.analyze_safety(expression) == %{expression | name: "checked_mod"}
    end

    test "% with divisor not spanning 0" do
      expression =
        function("%", [column_in_bounds({10, 20}), column_in_bounds({-100, -10})])
        |> set_bounds({0, 100})

      assert BoundAnalysis.analyze_safety(expression) == %{expression | name: "unsafe_mod"}
    end

    test "% with too large output bounds" do
      expression =
        function("%", [column_in_bounds({10, 20}), column_in_bounds({-100, -10})])
        |> set_bounds({0, @max_int + 1})

      assert BoundAnalysis.analyze_safety(expression) == expression
    end

    test "integer expression with result within 64bit unsigned bounds" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a]) |> set_bounds({100, 200})
      assert %Expression{name: "unsafe_add", args: [^a, ^a]} = BoundAnalysis.analyze_safety(expression)
    end

    test "integer expression with result outside of 64bit unsigned bounds" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a]) |> set_bounds({0, @max_int + 1})
      assert ^expression = BoundAnalysis.analyze_safety(expression)
    end

    test "real expression with magnitude of result smaller than 1.0e100" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a], :real) |> set_bounds({0, 1.0e50})
      assert %Expression{name: "unsafe_add", args: [^a, ^a]} = BoundAnalysis.analyze_safety(expression)
    end

    test "real expression with magnitude of result larger than 1.0e100" do
      a = column_in_bounds({10, 20})
      expression = function("+", [a, a], :real) |> set_bounds({0, 1.0e101})
      assert ^expression = BoundAnalysis.analyze_safety(expression)
    end

    test "^ that is out of bounds" do
      a = column_in_bounds({-20, 20})
      b = column_in_bounds({-1, 1})
      expression = function("^", [a, b], :real) |> set_bounds({0, 1.0e300})
      assert %Expression{name: "^", args: [^a, ^b]} = BoundAnalysis.analyze_safety(expression)
    end

    test "^ that could result in a complex number" do
      a = column_in_bounds({-20, 20})
      b = column_in_bounds({-1, 1})
      expression = function("^", [a, b], :real) |> set_bounds({-20, 20})
      assert %Expression{name: "checked_pow", args: [^a, ^b]} = BoundAnalysis.analyze_safety(expression)
    end

    test "^ that is totally safe" do
      a = column_in_bounds({10, 20})
      b = column_in_bounds({1, 2})
      expression = function("^", [a, b], :real) |> set_bounds({100, 200})
      assert %Expression{name: "unsafe_pow", args: [^a, ^b]} = BoundAnalysis.analyze_safety(expression)
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
    gen all(a <- integer(), b <- integer()) do
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
