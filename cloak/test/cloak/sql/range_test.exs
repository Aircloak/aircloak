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

    test "other range functions"
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
          ]
        )
      }
    }
  end
end
