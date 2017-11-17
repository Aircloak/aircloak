defmodule Cloak.Sql.Compiler.TypeChecker.Test do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser, Compiler.TypeChecker}

  describe "records used functions" do
    test "records usage of single functions", do:
      assert type_first_column("SELECT abs(numeric) FROM table").applied_functions == ["abs"]

    test "records usage of math functions", do:
      assert type_first_column("SELECT numeric + numeric FROM table").applied_functions == ["+"]

    test "records functions used across subqueries", do:
      assert type_first_column("SELECT c FROM (SELECT abs(numeric) as c FROM table) t").applied_functions == ["abs"]

    test "records multiple functions top down", do:
      assert type_first_column("SELECT abs(numeric + numeric) FROM table").applied_functions == ["abs", "+"]
  end

  describe "constant detection" do
    test "not constant if no constant appears", do:
      refute constant_involved?("SELECT numeric FROM table")

    test "not constant if math on two columns", do:
      refute constant_involved?("SELECT numeric + numeric FROM table")

    test "two math operations are considered a constant", do:
      assert constant_involved?("SELECT numeric + (numeric * numeric) FROM table")

    test "constant only input to a function is considered a constant" do
      assert constant?("SELECT abs(1) FROM table")
      assert constant?("SELECT 1 + 1 FROM table")
    end

    test "constant involved if an user provided constant is an input to a function", do:
      assert constant_involved?("SELECT numeric + 1 FROM table")

    test "touched by constant if a function input is deemed to be a potential constant", do:
      assert constant_involved?("SELECT left(string, numeric + (numeric * numeric)) FROM table")
  end

  describe "knows which columns were involved" do
    test "when a column is selected" do
      type = type_first_column("SELECT numeric FROM table")
      assert expression_name(type.history_of_columns_involved) == ["numeric"]
    end

    test "when a column is selected inside a function" do
      type = type_first_column("SELECT abs(numeric) FROM table")
      assert expression_name(type.history_of_columns_involved) == ["numeric"]
    end

    test "when a column is selected in a subquery" do
      type = type_first_column("SELECT col FROM (SELECT uid, numeric as col FROM table) t")
      assert expression_name(type.history_of_columns_involved) == ["numeric"]
    end

    test "when multiple columns are selected" do
      type = type_first_column("SELECT concat(string, cast(numeric as text)) FROM table")
      assert expression_name(type.history_of_columns_involved) == ["string", "numeric"]
    end

    test "deduplicates columns" do
      type = type_first_column("SELECT concat(string, string) FROM table")
      assert expression_name(type.history_of_columns_involved) == ["string"]
    end

    def expression_name(columns), do:
      columns
      |> Enum.map(& &1.name)
  end

  describe "records a history of dangerous functions" do
    test "empty history for queries without functions or math" do
      type = type_first_column("SELECT numeric FROM table")
      assert type.history_of_dangerous_transformations == []
    end

    test "for function with discontinuious function div" do
      type = type_first_column("SELECT div(numeric, 10) FROM table")
      assert type.history_of_dangerous_transformations == [{:dangerous_function, "div"}]
    end

    test "even when multiple occur" do
      type = type_first_column("SELECT abs(div(numeric, 10)) FROM table")
      assert type.history_of_dangerous_transformations ==
        [{:dangerous_function, "abs"}, {:dangerous_function, "div"}]
    end

    test "does not record discontinuous functions that aren't dangerous" do
      type = type_first_column("SELECT div(cast(sqrt(numeric) as integer), 10) FROM table")
      assert type.history_of_dangerous_transformations == [{:dangerous_function, "div"}]
    end

    test "records math influenced by a constant as a potential offense" do
      type = type_first_column("SELECT numeric + 10 FROM table")
      assert type.history_of_dangerous_transformations == [{:dangerous_function, "+"}]
    end

    test "does not record math between non-constant influenced columns" do
      type = type_first_column("SELECT numeric + numeric FROM table")
      assert type.history_of_dangerous_transformations == []
    end

    test "records multiple math offenses" do
      type = type_first_column("SELECT numeric + 10 FROM (SELECT uid, numeric - 1 as numeric FROM table) t")
      assert type.history_of_dangerous_transformations ==
        [{:dangerous_function, "+"}, {:dangerous_function, "-"}]
    end

    test "records multiple instances of the same offense" do
      type = type_first_column("SELECT numeric + 10 FROM (SELECT uid, numeric + 1 as numeric FROM table) t")
      assert type.history_of_dangerous_transformations ==
        [{:dangerous_function, "+"}, {:dangerous_function, "+"}]
    end

    test "records dangerous functions when it believes a constant has been constructed" do
      type = type_first_column("""
        SELECT abs(div(numeric, numeric) + div(numeric, numeric)) FROM table
      """)
      assert type.history_of_dangerous_transformations ==
        [{:dangerous_function, "abs"}, {:dangerous_function, "+"}]
    end
  end

  describe "IN" do
    test "allows clear IN lhs", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric IN (1, 2, 3)")

    test "forbids unclear IN lhs", do:
      assert {:error, "The left-hand side of an IN operator must be an unmodified database column."} =
        compile("SELECT COUNT(*) FROM table WHERE numeric + 1 IN (1, 2, 3)")

    test "allows clear IN lhs from subqueries", do:
      assert {:ok, _, _} =
        compile("SELECT COUNT(*) FROM (SELECT numeric AS number FROM table) x WHERE number IN (1, 2, 3)")

    test "forbids unclear IN lhs from subqueries", do:
      assert {:error, "The left-hand side of an IN operator must be an unmodified database column."} =
        compile("SELECT COUNT(*) FROM (SELECT numeric + 1 AS number FROM table) x WHERE number IN (1, 2, 3)")
  end

  describe "negative conditions" do
    test "allows clear <> lhs", do:
      assert {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric <> 10")

    test "forbids unclear <> lhs", do:
      assert {:error, "The <> operation can only be applied to an unmodified database column and a constant."} =
        compile("SELECT COUNT(*) FROM table WHERE numeric + 1 <> 10")

    test "forbids column <> column", do:
      assert {:error, "The <> operation can only be applied to an unmodified database column and a constant."} =
        compile("SELECT COUNT(*) FROM table WHERE numeric <> numeric")

    test "allows clear <> lhs in subquery HAVING", do:
      assert {:ok, _, _} = compile("""
        SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(numeric) <> 10) x
      """)

    test "forbids unclear <> lhs in subquery HAVING", do:
      assert {:error, "The <> operation can only be applied to an unmodified database column" <> _} = compile("""
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

  Enum.each(~w(constant_involved? constant?)a, fn(param) ->
    defp unquote(param)(query), do:
      type_first_column(query).unquote(param)
  end)

  defp type_first_column(query) do
    compiled_query = compile!(query)
    TypeChecker.test_establish_type(hd(compiled_query.columns), compiled_query)
  end

  defp compile!(query_string) do
    {:ok, result, _features} = compile(query_string)
    result
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
