defmodule Cloak.Sql.Compiler.TypeChecker.Test do
  @moduledoc false

  use ExUnit.Case, async: true

  alias Cloak.DataSource.Table
  alias Cloak.Sql.{Compiler, Parser}

  describe "IN" do
    test "allows clear IN lhs",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric IN (1, 2, 3)"))

    test "forbids unclear IN lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE numeric + 1 IN (1, 2, 3)")

      assert message =~ ~r[Only `lower`, `upper`, .*, and `.*` can be used in the left-hand side of an IN operator]
    end

    test "allows clear IN lhs from subqueries",
      do:
        assert(
          {:ok, _, _} =
            compile("SELECT COUNT(*) FROM (SELECT numeric AS number FROM table) x WHERE number IN (1, 2, 3)")
        )

    test "forbids unclear IN lhs from subqueries" do
      assert {:error, message} =
               compile("SELECT COUNT(*) FROM (SELECT numeric + 1 AS number FROM table) x WHERE number IN (1, 2, 3)")

      assert message =~ ~r[Only .* can be used in the left-hand side of an IN operator]
    end

    for function <- ~w(lower upper trim ltrim btrim extract_words) do
      test "allows #{function} in IN lhs" do
        assert {:ok, _, _} = compile("SELECT #{unquote(function)}(string) AS x FROM table WHERE x IN ('a', 'b', 'c')")
      end
    end

    test "allows substring in IN lhs" do
      assert {:ok, _, _} = compile("SELECT SUBSTRING(string FROM 3) AS x FROM table WHERE x IN ('a', 'b', 'c')")
    end
  end

  describe "negative conditions" do
    test "allows clear <> lhs", do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric <> 10"))

    test "forbids unclear <> lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE numeric + 1 <> 10")
      assert message =~ ~r[Only .* can be used in the arguments of an <> operator]
    end

    test "allows column <> column",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric <> numeric"))

    test "forbids column <> unclear_column" do
      assert {error, {1, 44}} = error_with_location("SELECT COUNT(*) FROM table WHERE string <> upper(string)")

      assert error =~
               ~r/No functions or mathematical operations are allowed when comparing two database columns with `<>`./
    end

    test "forbids unclear_column <> column" do
      assert {error, {1, 34}} = error_with_location("SELECT COUNT(*) FROM table WHERE upper(string) <> string")

      assert error =~
               ~r/No functions or mathematical operations are allowed when comparing two database columns with `<>`./
    end

    test "allows clear <> lhs in subquery HAVING",
      do:
        assert(
          {:ok, _, _} =
            compile("""
              SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(numeric) <> 10) x
            """)
        )

    test "forbids unclear <> lhs in subquery HAVING" do
      assert {:error, message} =
               compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING AVG(numeric + 1) <> 10) x")

      assert message =~ ~r[Only .* can be used in the arguments of an <> operator]
    end

    test "allows clear NOT LIKE lhs",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE string NOT LIKE '%some pattern_'"))

    test "allows clear NOT ILIKE lhs",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE string NOT ILIKE '%some pattern_'"))

    test "forbids unclear NOT LIKE lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE upper(string) NOT LIKE '%some pattern_'")

      assert message =~ ~r/Expressions with NOT LIKE cannot include any functions/
    end

    test "forbids unclear NOT ILIKE lhs" do
      assert {:error, message} = compile("SELECT COUNT(*) FROM table WHERE upper(string) NOT ILIKE '%some pattern_'")

      assert message =~ ~r/Expressions with NOT ILIKE cannot include any functions/
    end

    for function <- ~w(lower upper trim ltrim btrim extract_words) do
      test "allows #{function} in <> lhs" do
        assert {:ok, _, _} = compile("SELECT #{unquote(function)}(string) AS x FROM table WHERE x <> 'a'")
      end
    end

    test "allows substring in <> lhs" do
      assert {:ok, _, _} = compile("SELECT SUBSTRING(string FROM 3) AS x FROM table WHERE x <> 'a'")
    end
  end

  describe "string-based conditions" do
    test "allows string manipulation functions on clear columns in positive conditions",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE ltrim(string, 'abc') = 'foo'"))

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

    test "forbids complex expressions on the RHS of conditions with string manipulation functions",
      do:
        assert(
          {:error, "Results of string manipulation functions can only be compared to constants." <> _} =
            compile("SELECT COUNT(*) FROM table WHERE substring(string from 1) = lower(string)")
        )

    test "allows raw cast columns on the RHS of conditions with string manipulation functions",
      do:
        assert(
          {:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE substring(string from 1) = cast(numeric as text)")
        )

    test "allows string manipulation functions on clear columns in negative conditions",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE ltrim(string, 'abc') <> 'foo'"))

    test "forbids string manipulation functions on unclear columns in negative conditions",
      do: assert({:error, _} = compile("SELECT COUNT(*) FROM table WHERE btrim(string || string, 'abc') <> 'foo'"))

    test "allows string manipulation functions after a cast",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE btrim(cast(numeric as text), 'abc') = 'foo'"))

    test "forbids string manipulation functions after nested casts",
      do:
        assert(
          {:error, _} =
            compile("SELECT COUNT(*) FROM table WHERE btrim(cast(cast(numeric as real) as text), 'abc') = 'foo'")
        )

    test "allows string-based functions after aggregator",
      do:
        assert(
          {:ok, _, _} =
            compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING left(max(string), 3) = 'foo') x")
        )

    test "allows string-based functions before aggregator",
      do:
        assert(
          {:ok, _, _} =
            compile("SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING max(left(string, 3)) = 'foo') x")
        )
  end

  describe "ranges" do
    test "allows clear >=/< arguments",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric > 0 AND numeric < 10"))

    test "forbids unclear >=/< arguments" do
      assert {:error, narrative} = compile("SELECT COUNT(*) FROM table WHERE sqrt(numeric) > 0 AND sqrt(numeric) < 10")

      assert narrative =~ ~r/Range expressions cannot include any functions/
    end

    test "allows clear between arguments",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE numeric BETWEEN 0 AND 10"))

    test "forbids unclear between arguments" do
      assert {:error, narrative} = compile("SELECT COUNT(*) FROM table WHERE sqrt(numeric) BETWEEN 0 AND 10")

      assert narrative =~ ~r/Range expressions cannot include any functions/
    end

    test "allows any ranges in top-level HAVING",
      do:
        assert(
          {:ok, _, _} =
            compile("""
              SELECT COUNT(*) FROM table GROUP BY numeric HAVING sqrt(COUNT(float)) BETWEEN 0 AND 10
            """)
        )

    test "allows clear ranges in subquery HAVING",
      do:
        assert(
          {:ok, _, _} =
            compile("""
              SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING COUNT(float) BETWEEN 0 AND 10) x
            """)
        )

    test "forbids unclear ranges in subquery HAVING" do
      assert {:error, narrative} =
               compile("""
                 SELECT COUNT(*) FROM (SELECT uid FROM table GROUP BY uid HAVING sqrt(COUNT(float)) BETWEEN 0 AND 10) x
               """)

      assert narrative =~ ~r/Range expressions cannot include any functions/
    end

    test "forbids implicit ranges within another function" do
      assert {:error, narrative} = compile("SELECT abs(trunc(float)) FROM table")
      assert narrative =~ ~r/Range expressions cannot include any functions/
    end

    test "forbids nested implicit ranges" do
      assert {:error, narrative} = compile("SELECT trunc(trunc(float), -11) FROM table")
      assert narrative =~ ~r/Range expressions cannot include any functions/
    end

    test "forbids implicit ranges on function expressions" do
      assert {:error, narrative} = compile("SELECT trunc(float + 1) FROM table")
      assert narrative =~ ~r/Range expressions cannot include any functions/
    end

    test "does not consider cast to integer as an implicit range",
      do: assert({:ok, _, _} = compile("SELECT cast(float + 1 as integer) FROM table"))

    for function <- ~w(floor ceil ceiling) do
      test "does not consider #{function} as an implicit range",
        do: assert({:ok, _, _} = compile("SELECT #{unquote(function)}(float + 1) FROM table"))
    end

    test "allows casts in ranges",
      do: assert({:ok, _, _} = compile("SELECT COUNT(*) FROM table WHERE CAST(string AS INTEGER) BETWEEN 0 AND 10"))
  end

  describe "exceptions" do
    for function <- ~w(upper lower ltrim btrim rtrim) do
      test "#{function} is allowed with IN" do
        assert {:ok, _, _} =
                 compile("SELECT COUNT(*) FROM table WHERE #{unquote(function)}(string) IN ('foo', 'bar', 'baz')")
      end

      test "#{function} is allowed with NOT IN" do
        assert {:ok, _, _} =
                 compile("SELECT COUNT(*) FROM table WHERE #{unquote(function)}(string) NOT IN ('foo', 'bar', 'baz')")
      end
    end

    test "substring is allowed with IN" do
      assert {:ok, _, _} =
               compile("SELECT COUNT(*) FROM table WHERE substring(string FROM 1 FOR 10) IN ('foo', 'bar', 'baz')")
    end

    test "substring is allowed with NOT IN" do
      assert {:ok, _, _} =
               compile("SELECT COUNT(*) FROM table WHERE substring(string FROM 1 FOR 10) NOT IN ('foo', 'bar', 'baz')")
    end
  end

  defp error_with_location(query_string) do
    Compiler.compile!(data_source(), Parser.parse!(query_string), [], %{})
    flunk("Expected an error")
  rescue
    e in Cloak.Sql.CompilationError ->
      {e.message, e.source_location}
  end

  defp compile(query_string), do: Compiler.compile(data_source(), Parser.parse!(query_string), [], %{})

  defp data_source() do
    %{
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
              Table.column("float", :real),
              Table.column("string", :text),
              Table.column("time", :time),
              Table.column("date", :date)
            ]
          )
      }
    }
  end
end
