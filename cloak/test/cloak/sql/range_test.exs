defmodule Cloak.Sql.Range.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser, Range, Query}

  describe "find_ranges" do
    test ">/<= ranges" do
      query = compile("SELECT COUNT(*) FROM table WHERE number > 0 AND number <= 10")
      assert [%Range{column: %{name: "number"}, interval: {0, 10}}] = Range.find_ranges(query)
    end

    test ">/<= ranges in subquery having" do
      %{from: {:subquery, %{ast: subquery}}} =
        compile("""
          SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING AVG(number) > 10 AND AVG(number) < 20) x
        """)

      assert [
               %Range{
                 column: %{function: "avg", function_args: [%{name: "number"}]},
                 interval: {10, 20}
               }
             ] = Range.find_ranges(subquery)
    end

    test "invalid ranges" do
      %{from: {:subquery, %{ast: subquery}}} =
        compile("""
          SELECT COUNT(*) FROM (SELECT uid FROM table SAMPLE_USERS 10%) x
        """)
        |> Compiler.Execution.align()

      assert [%Range{interval: :invalid}] = Range.find_ranges(subquery)
    end

    test "function range in SELECT" do
      query = compile("SELECT month(timestamp) FROM table")

      assert [%Range{column: %{function: "month"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "function range in expression in SELECT" do
      query = compile("SELECT month(timestamp) + 1 FROM table")
      assert [%Range{column: %{function: "+"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "function range on expression in SELECT" do
      query = compile("SELECT trunc(float + 1) FROM table")

      assert [%Range{column: %{function: "trunc"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "function range in WHERE" do
      query = compile("SELECT COUNT(*) FROM table WHERE trunc(float) = 10")

      assert [%Range{column: %{function: "trunc"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "no function ranges from top-level HAVING" do
      query = compile("SELECT COUNT(*) FROM table GROUP BY number HAVING trunc(float) = 10")
      assert [] = Range.find_ranges(query)
    end

    test "function ranges from subquery HAVING" do
      %{from: {:subquery, %{ast: subquery}}} =
        compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid, number HAVING trunc(float) = 10) x")

      assert [%Range{column: %{function: "trunc"}, interval: :implicit}] = Range.find_ranges(subquery)
    end

    test "top-level selected aggregates are not ranges" do
      query = compile("SELECT round(AVG(number)) + 1 FROM table")
      assert [] = Range.find_ranges(query)
    end

    test "top-level selected constants are not ranges" do
      query = compile("SELECT 2 + 3 FROM table")
      assert [] = Range.find_ranges(query)
    end

    test "subquery selected aggregates normally considered for ranges" do
      query =
        compile("""
          SELECT COUNT(*) FROM (SELECT uid, round(AVG(number)) AS rounded FROM table GROUP BY uid) x WHERE rounded = 10
        """)

      assert [%Range{column: %{name: "rounded"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "subquery selected columns later ignored" do
      query = compile("SELECT COUNT(*) FROM (SELECT uid, trunc(float) AS trunced FROM table) x")
      assert [] = Range.find_ranges(query)
    end

    test "subquery selected columns later filtered" do
      query = compile("SELECT COUNT(*) FROM (SELECT uid, trunc(float) AS trunced FROM table) x WHERE trunced = 10")

      assert [%Range{column: %{name: "trunced"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "subquery selected columns later selected top-level" do
      query = compile("SELECT trunced FROM (SELECT uid, trunc(float) AS trunced FROM table) x")
      assert [%Range{column: %{name: "trunced"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "subquery distinct aggregate later selected top-level" do
      query =
        compile("""
          SELECT result FROM (SELECT uid, count(distinct round(float)) AS result FROM table GROUP BY uid) x
        """)

      assert [%Range{column: %{name: "result"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "range_function() + range_function()" do
      query = compile("SELECT trunc(float) + trunc(float) FROM table")
      assert [%Range{column: %{function: "+"}, interval: :implicit}] = Range.find_ranges(query)
    end

    for function <- ~w(round trunc) do
      test "#{function} range in SELECT" do
        query = compile("SELECT #{unquote(function)}(number, 2) FROM table")

        assert [%Range{column: %{function: unquote(function)}, interval: :implicit}] = Range.find_ranges(query)
      end
    end

    for part <- ~w(year quarter month day hour minute second) do
      test "date_trunc(#{part})" do
        query = compile("SELECT date_trunc('#{unquote(part)}', timestamp) FROM table")

        assert [%Range{column: %{function: "date_trunc"}, interval: :implicit}] = Range.find_ranges(query)
      end
    end
  end

  describe "range?" do
    test "false for regular conditions" do
      query = compile("SELECT COUNT(*) FROM table WHERE number = 3")
      refute Range.range?(query.where, query)
    end

    test "true for inequalities" do
      query = compile("SELECT COUNT(*) FROM table WHERE number BETWEEN 0 AND 10")

      for condition <- get_in(query.where, [Query.Lenses.conditions()]) do
        assert Range.range?(condition, query)
      end
    end

    test "true for implicit ranges" do
      query = compile("SELECT COUNT(*) FROM table WHERE round(number, 2) = 10")
      assert Range.range?(query.where, query)
    end

    test "subqueries" do
      query = compile("SELECT COUNT(*) FROM (SELECT uid, round(number, 2) AS foo FROM table) bar WHERE foo = 10")
      assert Range.range?(query.where, query)
    end
  end

  defp compile(query_string) do
    query_string
    |> Parser.parse!()
    |> Compiler.Specification.compile(nil, data_source(), _parameters = [], _views = %{})
  end

  defp data_source() do
    %{
      name: "data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("number", :integer),
              Table.column("float", :real),
              Table.column("timestamp", :datetime)
            ]
          )
      }
    }
  end
end
