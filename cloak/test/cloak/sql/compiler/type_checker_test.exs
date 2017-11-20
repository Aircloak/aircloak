defmodule Cloak.Sql.Compiler.TypeChecker.Test do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser}

  describe "IN" do
    test "allows clear IN lhs", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric IN (1, 2, 3)")

    test "forbids unclear IN lhs", do:
      assert {:error, "Only `lower`, `upper` can be used in the left-hand side of an IN operator."} =
        compile("SELECT COUNT(*) FROM table WHERE numeric + 1 IN (1, 2, 3)")

    test "allows clear IN lhs from subqueries", do:
      assert {:ok, _, _} =
        compile("SELECT COUNT(*) FROM (SELECT numeric AS number FROM table) x WHERE number IN (1, 2, 3)")

    test "forbids unclear IN lhs from subqueries", do:
      assert {:error, "Only `lower`, `upper` can be used in the left-hand side of an IN operator."} =
        compile("SELECT COUNT(*) FROM (SELECT numeric + 1 AS number FROM table) x WHERE number IN (1, 2, 3)")
  end

  describe "negative conditions" do
    test "allows clear <> lhs", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric <> 10")

    test "forbids unclear <> lhs", do:
      assert {:error, "Only `lower`, `upper` can be used in the left-hand side of an <> operator."} =
        compile("SELECT COUNT(*) FROM table WHERE numeric + 1 <> 10")

    test "forbids column <> column", do:
      assert {:error, "The right-hand side of an <> operator has to be a constant."} =
        compile("SELECT COUNT(*) FROM table WHERE numeric <> numeric")

    test "allows clear <> lhs in subquery HAVING", do:
      assert {:ok, _, _} = compile("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(numeric) <> 10) x
      """)

    test "forbids unclear <> lhs in subquery HAVING", do:
      assert {:error, "Only `lower`, `upper` can be used in the left-hand side of an <> operator."} = compile("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING AVG(numeric + 1) <> 10) x
      """)

    test "allows clear NOT LIKE lhs", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE string NOT LIKE '%some pattern_'")

    test "allows clear NOT ILIKE lhs", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE string NOT ILIKE '%some pattern_'")

    test "forbids unclear NOT LIKE lhs", do:
      assert {:error, "NOT LIKE can only be applied to an unmodified database column."} =
        compile("SELECT COUNT(*) FROM table WHERE upper(string) NOT LIKE '%some pattern_'")

    test "forbids unclear NOT ILIKE lhs", do:
      assert {:error, "NOT ILIKE can only be applied to an unmodified database column."} =
        compile("SELECT COUNT(*) FROM table WHERE upper(string) NOT ILIKE '%some pattern_'")
  end

  describe "ranges" do
    test "allows clear >=/< arguments", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric > 0 AND numeric < 10")

    test "forbids unclear >=/< arguments", do:
      assert {:error, "Only unmodified database columns can be limited by a range."} =
        compile("SELECT COUNT(*) FROM table WHERE sqrt(numeric) > 0 AND sqrt(numeric) < 10")

    test "allows clear between arguments", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric BETWEEN 0 AND 10")

    test "forbids unclear between arguments", do:
      assert {:error, "Only unmodified database columns can be limited by a range."} =
        compile("SELECT COUNT(*) FROM table WHERE sqrt(numeric) BETWEEN 0 AND 10")

    test "allows any ranges in top-level HAVING", do:
      assert {:ok, _, _} = compile("""
        SELECT COUNT(*) FROM table GROUP BY numeric HAVING sqrt(COUNT(float)) BETWEEN 0 AND 10
      """)

    test "allows clear ranges in subquery HAVING", do:
      assert {:ok, _, _} = compile("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(float) BETWEEN 0 AND 10) x
      """)

    test "forbids unclear ranges in subquery HAVING", do:
      assert {:error,  "Only unmodified database columns can be limited by a range."} = compile("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING sqrt(COUNT(float)) BETWEEN 0 AND 10) x
      """)

    test "allows casts in ranges", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE CAST(string AS INTEGER) BETWEEN 0 AND 10")
  end

  describe "exceptions" do
    for function <- ~w(upper lower) do
      test "#{function} is allowed with IN" do
        assert {:ok, _, _} =
          compile("SELECT COUNT(*) FROM table WHERE #{unquote(function)}(string) IN ('foo', 'bar', 'baz')")
      end

      test "#{function} is allowed with NOT IN" do
        assert {:ok, _, _} =
          compile("SELECT COUNT(*) FROM table WHERE #{unquote(function)}(string) NOT IN ('foo', 'bar', 'baz')")
      end
    end
  end

  defp compile(query_string), do:
    Compiler.compile(data_source(), Parser.parse!(query_string), [], %{})

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Cloak.DataSource.Table.new("table", "uid",
          db_name: "table",
          columns: [
            Table.column("uid", :integer),
            Table.column("numeric", :integer),
            Table.column("float", :real),
            Table.column("string", :text),
            Table.column("time", :time),
            Table.column("date", :date),
          ]
        )
      }
    }
  end
end
