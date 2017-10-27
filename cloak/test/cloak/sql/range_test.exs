defmodule Cloak.Sql.Range.Test do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser, Range}

  describe "find_ranges" do
    test ">/<= ranges" do
      query = compile("SELECT COUNT(*) FROM table WHERE number > 0 AND number <= 10")
      assert [%Range{column: %{name: "number"}, interval: {0, 10}}] = Range.find_ranges(query)
    end

    test ">/<= ranges in subquery having" do
      %{from: {:subquery, %{ast: subquery}}} = compile("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING AVG(number) > 10 AND AVG(number) < 20) x
      """)

      assert [%Range{column: %{function: "avg", function_args: [%{name: "number"}]}, interval: {10, 20}}] =
        Range.find_ranges(subquery)
    end

    test "invalid ranges" do
      %{from: {:subquery, %{ast: subquery}}} = compile("""
        SELECT COUNT(*) FROM (SELECT uid FROM table SAMPLE_USERS 10%) x
      """)
      |> Compiler.Execution.prepare()

      assert [%Range{interval: :invalid}] = Range.find_ranges(subquery)
    end

    test "function range in SELECT" do
      query = compile("SELECT month(timestamp) FROM table")
      assert [%Range{column: %{name: "timestamp"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "function range in expression in SELECT" do
      query = compile("SELECT month(timestamp) + 1 FROM table")
      assert [%Range{column: %{name: "timestamp"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "function range on expression in SELECT" do
      query = compile("SELECT trunc(number + 1) FROM table")
      assert [%Range{column: %{function: "+"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "function range in WHERE" do
      query = compile("SELECT COUNT(*) FROM table WHERE trunc(number) = 10")
      assert [%Range{column: %{name: "number"}, interval: :implicit}] = Range.find_ranges(query)
    end

    test "no function ranges from top-level HAVING" do
      query = compile("SELECT COUNT(*) FROM table GROUP BY number HAVING trunc(number) = 10")
      assert [] = Range.find_ranges(query)
    end

    test "no function ranges from subquery HAVING" do
      %{from: {:subquery, %{ast: subquery}}} =
        compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid, number HAVING trunc(number) = 10) x")
      assert [%Range{column: %{name: "number"}, interval: :implicit}] = Range.find_ranges(subquery)
    end

    for function <- ~w(round trunc) do
      test "#{function} range in SELECT" do
        query = compile("SELECT #{unquote(function)}(number, 2) FROM table")
        assert [%Range{column: %{name: "number"}, interval: :implicit}] = Range.find_ranges(query)
      end
    end

    for part <- ~w(year quarter month day hour minute second) do
      test "date_trunc(#{part})" do
        query = compile("SELECT date_trunc('#{unquote(part)}', timestamp) FROM table")
        assert [%Range{column: %{name: "timestamp"}, interval: :implicit}] = Range.find_ranges(query)
      end
    end
  end

  defp compile(query_string) do
    query_string
    |> Parser.parse!()
    |> Compiler.ASTNormalization.normalize()
    |> Compiler.Specification.compile(data_source(), _parameters = [], _views = %{})
  end

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Table.new("table", "uid",
          db_name: "table",
          columns: [
            Table.column("uid", :integer),
            Table.column("number", :integer),
            Table.column("timestamp", :datetime),
          ]
        )
      }
    }
  end
end
