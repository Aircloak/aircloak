defmodule Cloak.Sql.Compiler.TypeChecker.Test do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser}

  describe "IN" do
    test "allows clear IN lhs",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE round(numeric) IN (1, 2, 3)"))

    test "forbids unclear IN lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE numeric * 2 IN (1, 2, 3)")

      assert message =~ ~r[Only clear expressions can be used on the left-hand side of an IN operator.]
    end

    test "allows clear IN lhs from subqueries",
      do:
        assert(
          {:ok, _} =
            compile("SELECT COUNT(*) FROM (SELECT numeric AS number FROM table) x WHERE round(number) IN (1, 2, 3)")
        )

    test "forbids unclear IN lhs from subqueries" do
      assert {:error, message} =
               compile("SELECT COUNT(*) FROM (SELECT numeric * 2 AS number FROM table) x WHERE number IN (1, 2, 3)")

      assert message =~ ~r[Only clear expressions can be used on the left-hand side of an IN operator.]
    end

    for function <- ~w(lower upper trim ltrim btrim) do
      test "allows #{function} in IN lhs" do
        assert {:ok, _} = compile("SELECT #{unquote(function)}(string) AS x FROM table WHERE x IN ('a', 'b', 'c')")
      end
    end

    for part <- ~w(hour minute second year quarter month day weekday dow) do
      test "allows extract(#{part}) in IN lhs" do
        assert {:ok, _} = compile("SELECT extract(#{unquote(part)} from datetime) AS x FROM table WHERE x IN (1, 2, 3)")
      end
    end

    test "allows substring in IN lhs" do
      assert {:ok, _} = compile("SELECT SUBSTRING(string FROM 3) AS x FROM table WHERE x IN ('a', 'b', 'c')")
    end
  end

  describe "negative conditions" do
    test "allows column <> lhs", do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric <> 10"))

    test "forbids unclear <> lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE numeric * 0 <> 10")
      assert message =~ ~r[Comparisons need to have clear expressions on both sides of the operator]
    end

    test "allows column <> column",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric <> numeric"))

    test "allows column <> clear expression",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE string <> upper(string)"))

    test "allow clear expression <> column",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE upper(string) <> string"))

    test "allow clear expression <> clear expression",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE lower(string) <> lower(string)"))

    test "forbids column <> unclear expression" do
      assert {:error, narrative} = compile("SELECT COUNT(*) FROM table WHERE string <> string || string")

      assert narrative =~ ~r/Comparisons need to have clear expressions on both sides of the operator/
    end

    test "allows clear <> lhs in subquery HAVING",
      do:
        assert(
          {:ok, _} =
            compile("""
              SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(numeric) <> 10) x
            """)
        )

    test "forbids unclear <> lhs in subquery HAVING" do
      assert {:error, message} =
               compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING AVG(numeric * 0) <> 10) x")

      assert message =~ ~r[Comparisons need to have clear expressions on both sides of the operator]
    end

    test "allows column NOT LIKE lhs",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE string NOT LIKE '%some pattern_'"))

    test "allows column NOT ILIKE lhs",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE string NOT ILIKE '%some pattern_'"))

    test "forbids function(column) NOT LIKE lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE upper(string) NOT LIKE '%some pattern_'")

      assert message =~ ~r/Expressions with `NOT LIKE` cannot include any functions/
    end

    test "forbids function(column) NOT ILIKE lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE upper(string) NOT ILIKE '%some pattern_'")

      assert message =~ ~r/Expressions with `NOT ILIKE` cannot include any functions/
    end

    for function <- ~w(lower upper trim ltrim btrim) do
      test "allows #{function} in <> lhs" do
        assert {:ok, _} = compile("SELECT #{unquote(function)}(string) AS x FROM table WHERE x <> 'a'")
      end
    end

    for part <- ~w(hour minute second year quarter month day weekday dow) do
      test "allows extract(#{part}) in <> lhs" do
        assert {:ok, _} = compile("SELECT extract(#{unquote(part)} from datetime) AS x FROM table WHERE x <> 1")
      end
    end

    test "allows substring in <> lhs" do
      assert {:ok, _} = compile("SELECT SUBSTRING(string FROM 3) AS x FROM table WHERE x <> 'a'")
    end
  end

  describe "string-based conditions" do
    test "allows string manipulation functions on clear columns in positive conditions",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE ltrim(string, 'abc') = 'foo'"))

    test "forbids string manipulation functions on unclear columns in top-level select",
      do:
        assert(
          {:error, "String manipulation functions cannot be combined with other transformations." <> _} =
            compile("SELECT btrim(string || string, 'abc') FROM table")
        )

    test "forbids string manipulation functions on unclear columns in positive conditions",
      do:
        assert(
          {:error, "String manipulation functions cannot be combined with other transformations." <> _} =
            compile("SELECT COUNT(*) FROM table WHERE btrim(string || string, 'abc') = 'foo'")
        )

    test "forbids operations after a string manipulation function",
      do:
        assert(
          {:error, "String manipulation functions cannot be combined with other transformations." <> _} =
            compile("SELECT COUNT(*) FROM table WHERE rtrim(string) || string = 'foo'")
        )

    test "forbids unclear expressions on the RHS of conditions with string manipulation functions",
      do:
        assert(
          {:error, "Results of string manipulation functions can only be compared to clear expressions." <> _} =
            compile("SELECT COUNT(*) FROM table WHERE substring(string from 1) = string || 'a'")
        )

    test "allow clear expressions conditions with string manipulation functions",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE ltrim(string) = rtrim(string)"))

    test "allows raw cast columns on the RHS of conditions with string manipulation functions",
      do:
        assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE substring(string from 1) = cast(numeric as text)"))

    test "allows string manipulation functions on clear columns in negative conditions",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE ltrim(string, 'abc') <> 'foo'"))

    test "forbids string manipulation functions on unclear columns in negative conditions",
      do: assert({:error, _} = compile("SELECT COUNT(*) FROM table WHERE btrim(string || string, 'abc') <> 'foo'"))

    test "allows string manipulation functions after a cast",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE btrim(cast(numeric as text), 'abc') = 'foo'"))

    test "forbids string manipulation functions after nested casts",
      do:
        assert(
          {:error, _} =
            compile("SELECT COUNT(*) FROM table WHERE btrim(cast(cast(numeric as real) as text), 'abc') = 'foo'")
        )

    test "allows string-based functions after aggregator",
      do:
        assert(
          {:ok, _} =
            compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING left(max(string), 3) = 'foo') x")
        )

    test "allows string-based functions before aggregator",
      do:
        assert(
          {:ok, _} =
            compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING max(left(string, 3)) = 'foo') x")
        )

    test "allows string manipulation functions in top-level having",
      do:
        assert({:ok, _} = compile("SELECT string, COUNT(*) FROM table GROUP BY 1 HAVING btrim(left(string, 1)) = 'f'"))
  end

  describe "ranges" do
    test "allows clear >=/< arguments",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric > 0 AND numeric < 10"))

    test "forbids unclear >=/< arguments" do
      assert {:error, narrative} = compile("SELECT COUNT(*) FROM table WHERE sqrt(numeric) > 0 AND sqrt(numeric) < 10")

      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    for operator <- ~w(> < >= <=) do
      test "allows col1 #{operator} col2" do
        assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric #{unquote(operator)} numeric2")
      end

      test "allows col1 #{operator} round(col2)" do
        assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric #{unquote(operator)} round(numeric2)")
      end

      test "allows round(col1) #{operator} col2" do
        assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE round(numeric) #{unquote(operator)} numeric2")
      end

      test "allows round(col1, constant) #{operator} round(col2, constant)" do
        assert {:ok, _} =
                 compile("SELECT COUNT(*) FROM table WHERE round(numeric, -1) #{unquote(operator)} round(numeric2, -1)")
      end

      test "forbids unclear expression #{operator} column" do
        assert {:error, narrative} =
                 compile("SELECT COUNT(*) FROM table WHERE numeric - numeric #{unquote(operator)} numeric2")

        assert narrative =~ ~r/Comparisons need to have clear expressions on both sides of the operator/
      end
    end

    test "allows col1 BETWEEN col2 AND col3" do
      assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric BETWEEN numeric2 AND numeric3")
    end

    test "allow functions in col1 BETWEEN col2 AND col3" do
      assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric BETWEEN round(numeric2) AND numeric3")
    end

    test "allows clear between arguments",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE numeric BETWEEN 0 AND 10"))

    test "forbids unclear between arguments" do
      assert {:error, narrative} = compile("SELECT COUNT(*) FROM table WHERE sqrt(numeric) BETWEEN 0 AND 10")

      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    test "allows any ranges in top-level HAVING",
      do:
        assert(
          {:ok, _} =
            compile("""
              SELECT COUNT(*) FROM table GROUP BY numeric HAVING sqrt(COUNT(float)) BETWEEN 0 AND 10
            """)
        )

    test "allows clear ranges in subquery HAVING",
      do:
        assert(
          {:ok, _} =
            compile("""
              SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(float) BETWEEN 0 AND 10) x
            """)
        )

    test "forbids unclear ranges in subquery HAVING" do
      assert {:error, narrative} =
               compile("""
                 SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING sqrt(COUNT(float)) BETWEEN 0 AND 10) x
               """)

      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    test "forbids unclear implicit ranges on the lhs of condition" do
      assert {:error, narrative} = compile("SELECT count(*) FROM table WHERE trunc(float + 0.5) = 4")
      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    test "forbids unclear implicit ranges on the rhs of condition" do
      assert {:error, narrative} = compile("SELECT count(*) FROM table WHERE float = trunc(float + 0.5)")
      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    test "forbids implicit ranges within another function" do
      assert {:error, narrative} = compile("SELECT abs(trunc(float)) FROM table")
      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    test "forbids nested implicit ranges" do
      assert {:error, narrative} = compile("SELECT trunc(trunc(float), -11) FROM table")
      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    test "forbids implicit ranges on function expressions" do
      assert {:error, narrative} = compile("SELECT trunc(float + 1) FROM table")
      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    test "consider cast to integer as an implicit range" do
      assert({:error, narrative} = compile("SELECT cast(float + 1 as integer) FROM table"))
      assert narrative =~ ~r/Only clear expressions can be used in range conditions/
    end

    for function <- ~w(round trunc floor ceil) do
      test "consider #{function} as an implicit range" do
        assert({:error, narrative} = compile("SELECT #{unquote(function)}(float + 1) FROM table"))
        assert narrative =~ ~r/Only clear expressions can be used in range conditions/
      end
    end

    test "allows casts in ranges",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE CAST(string AS INTEGER) BETWEEN 0 AND 10"))

    for part <- ~w(hour minute second year quarter month day weekday dow) do
      test "allows extract(#{part}) in ranges" do
        assert {:ok, _} =
                 compile("SELECT extract(#{unquote(part)} from datetime) AS x FROM table WHERE x BETWEEN 1 AND 3")
      end
    end
  end

  describe "exceptions" do
    for function <- ~w(upper lower ltrim btrim rtrim) do
      test "#{function} is allowed with IN" do
        assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE #{unquote(function)}(string) IN ('foo', 'bar')")
      end

      test "#{function} is allowed with NOT IN" do
        assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE #{unquote(function)}(string) NOT IN ('foo', 'bar')")
      end
    end

    test "substring is allowed with IN" do
      assert {:ok, _} = compile("SELECT COUNT(*) FROM table WHERE substring(string FROM 1 FOR 10) IN ('foo', 'bar')")
    end

    test "substring is allowed with NOT IN" do
      assert {:ok, _} =
               compile("SELECT COUNT(*) FROM table WHERE substring(string FROM 1 FOR 10) NOT IN ('foo', 'bar')")
    end
  end

  describe "arbitray math" do
    test "allows arbitray math in top-level having" do
      assert {:ok, _} =
               compile(
                 "SELECT float, COUNT(*) FROM table GROUP BY 1 HAVING float * 3 / (3 - float) + 1 - float * 2 = 7"
               )
    end

    test "forbid arbitray math in where filter" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE float * 3 / (3 - float) + 1 - float * 2 = 7")
      assert "Queries containing expressions with a high number of functions" <> _ = message
    end
  end

  describe "IS NULL" do
    test "allows clear IS NULL",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE round(numeric) IS NULL"))

    test "allows clear IS NOT NULL",
      do: assert({:ok, _} = compile("SELECT COUNT(*) FROM table WHERE round(numeric) IS NOT NULL"))

    test "forbids unclear IS NULL" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE 1 / numeric IS NULL")

      assert message =~ ~r(Only clear expressions can be used with the `IS \[NOT\] NULL` operator\.)
    end

    test "forbids unclear IS NOT NULL" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE 1 / numeric IS NOT NULL")

      assert message =~ ~r(Only clear expressions can be used with the `IS \[NOT\] NULL` operator\.)
    end

    test "forbids unclear IS NULL from subqueries" do
      assert {:error, message} =
               compile("SELECT COUNT(*) FROM (SELECT 1 / numeric AS number FROM table) x WHERE number IS NULL")

      assert message =~ ~r(Only clear expressions can be used with the `IS \[NOT\] NULL` operator\.)
    end
  end

  describe "aggregators" do
    test "allows clear aggregator",
      do: assert({:ok, _} = compile("SELECT SUM(round(numeric)) FROM table"))

    test "forbids unclear count" do
      assert {:error, message} = compile("SELECT COUNT(1/numeric) FROM table")

      assert message =~ ~r(Only clear expressions can be aggregated\.)
    end

    test "forbids unclear sum" do
      assert {:error, message} = compile("SELECT SUM(1/numeric) FROM table")

      assert message =~ ~r(Only clear expressions can be aggregated\.)
    end

    test "forbids unclear count distinct" do
      assert {:error, message} = compile("SELECT COUNT(DISTINCT 1/numeric) FROM table")

      assert message =~ ~r(Only clear expressions can be aggregated\.)
    end

    test "forbids unclear aggregator from subquery" do
      assert {:error, message} = compile("SELECT STDDEV(number) FROM (SELECT 1 / numeric AS number FROM table) x")

      assert message =~ ~r(Only clear expressions can be aggregated\.)
    end
  end

  defp compile(query_string),
    do: query_string |> Parser.parse!() |> Compiler.compile(nil, data_source(), [], %{})

  defp data_source() do
    %{
      name: "type_checker_test_data_source",
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table:
          Cloak.DataSource.Table.new(
            "table",
            "uid",
            db_name: "table",
            columns: [
              Table.column("uid", :integer),
              Table.column("numeric", :integer),
              Table.column("numeric2", :integer),
              Table.column("numeric3", :integer),
              Table.column("float", :real),
              Table.column("string", :text),
              Table.column("time", :time),
              Table.column("date", :date),
              Table.column("datetime", :datetime)
            ]
          )
      }
    }
  end
end
