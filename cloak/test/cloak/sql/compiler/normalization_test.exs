defmodule Cloak.Sql.Compiler.Normalization.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Expression
  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  defmacrop assert_equivalent(query, alternative) do
    quote bind_quoted: [query: query, alternative: alternative] do
      assert scrub_locations(compile!(query, data_source())) == scrub_locations(compile!(alternative, data_source()))
    end
  end

  defmacrop refute_equivalent(query, alternative) do
    quote bind_quoted: [query: query, alternative: alternative] do
      refute compile!(query, data_source()) == compile!(alternative, data_source())
    end
  end

  setup_all do
    Cloak.TestShadowCache.safe(data_source(), "table", "numeric", [3])
  end

  describe "constant normalization" do
    test "normalizing constant expressions" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE numeric = 2 * 3 + 4",
        "SELECT STDDEV(uid) FROM table WHERE numeric = 10"
      )
    end

    test "normalizing constant expressions with NULL" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE numeric = 2 + NULL",
        "SELECT STDDEV(uid) FROM table WHERE numeric = NULL"
      )
    end

    test "[Issue #3384] normalizing constant bucket" do
      assert_equivalent(
        "SELECT bucket(2 BY 7) FROM table",
        "SELECT 0::real AS \"bucket\" FROM table"
      )
    end

    test "[Issue #3413] normalizing aggregators over constants" do
      assert_equivalent(
        "SELECT ABS(SUM(1 - 10)) FROM table",
        "SELECT ABS(SUM(-9)) FROM table"
      )
    end
  end

  test "normalization in subqueries" do
    assert_equivalent(
      "SELECT * FROM (SELECT STDDEV(uid) FROM table WHERE numeric = 2 * 3 + 4) x",
      "SELECT * FROM (SELECT STDDEV(uid) FROM table WHERE numeric = 10) x"
    )
  end

  test "normalizing trivial like patterns into =" do
    assert_equivalent(
      "SELECT STDDEV(uid) FROM table WHERE string LIKE 'abc'",
      "SELECT STDDEV(uid) FROM table WHERE string = 'abc'"
    )
  end

  test "normalizing trivial not like patterns into <>" do
    assert_equivalent(
      "SELECT STDDEV(uid) FROM table WHERE string NOT LIKE 'abc'",
      "SELECT STDDEV(uid) FROM table WHERE string <> 'abc'"
    )
  end

  test "normalizing trivial ilike patterns into =" do
    assert_equivalent(
      "SELECT STDDEV(uid) FROM table WHERE string ILIKE 'Abc'",
      "SELECT STDDEV(uid) FROM table WHERE lower(string) = 'abc'"
    )
  end

  test "normalizing trivial not ilike patterns <>" do
    assert_equivalent(
      "SELECT STDDEV(uid) FROM table WHERE string NOT ILIKE 'Abc'",
      "SELECT STDDEV(uid) FROM table WHERE lower(string) <> 'abc'"
    )
  end

  describe "remove noops" do
    test "a cast of integer to integer" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE cast(numeric AS integer) = 1",
        "SELECT STDDEV(uid) FROM table WHERE numeric = 1"
      )
    end

    for function <- ~w/round trunc/ do
      test "#{function} of integer without precision is removed" do
        assert_equivalent(
          "SELECT STDDEV(uid) FROM table WHERE #{unquote(function)}(numeric) = 1",
          "SELECT STDDEV(uid) FROM table WHERE numeric = 1"
        )
      end

      test "#{function} of integer with precision isn't removed" do
        refute_equivalent(
          "SELECT STDDEV(uid) FROM table WHERE #{unquote(function)}(numeric, 0) = 1",
          "SELECT STDDEV(uid) FROM table WHERE numeric = 1"
        )
      end
    end

    for function <- ~w/ceil ceiling floor/ do
      test "#{function} of integer is removed" do
        assert_equivalent(
          "SELECT STDDEV(uid) FROM table WHERE #{unquote(function)}(numeric) = 1",
          "SELECT STDDEV(uid) FROM table WHERE numeric = 1"
        )
      end
    end
  end

  describe "rewrite DISTINCT to GROUP BY" do
    test "fails on combination of DISTINCT, GROUP BY and ORDER BY" do
      assert {:error, error} =
               compile(
                 """
                   SELECT DISTINCT numeric
                   FROM table
                   GROUP BY numeric
                   ORDER BY numeric
                 """,
                 data_source()
               )

      assert error =~ ~r/Simultaneous usage of `DISTINCT`, `GROUP BY`, and `ORDER BY`/
    end

    test "Rejects non-aggregate queries with DISTINCT and GROUP BY where some but not all columns are grouped" do
      assert {:error, error} =
               compile(
                 """
                   SELECT DISTINCT numeric, string
                   FROM table
                   GROUP BY string
                 """,
                 data_source()
               )

      assert error =~ ~r/Column `numeric` needs to appear in the `GROUP BY` clause/
    end

    test "DISTINCT rewrite does not affect regular warning about missing GROUP BY" do
      assert {:error, error} =
               compile(
                 """
                   SELECT DISTINCT numeric, count(*)
                   FROM table
                 """,
                 data_source()
               )

      assert error =~ ~r/Column `numeric` needs to appear in the `GROUP BY` clause/
    end

    test "Rejects DISTINCT with aggregate where additional GROUP BY columns exist" do
      assert {:error, error} = compile("SELECT DISTINCT count(*) FROM table GROUP BY numeric", data_source())
      assert error =~ ~r/Grouping by unselected columns .* `DISTINCT`/
    end

    test "distinct on top-level query" do
      assert_equivalent(
        "SELECT DISTINCT numeric FROM table",
        "SELECT numeric FROM table GROUP BY numeric"
      )
    end

    test "distinct on top-level query with alias" do
      assert_equivalent(
        "SELECT DISTINCT numeric as a FROM table GROUP BY a",
        "SELECT numeric as a FROM table GROUP BY numeric"
      )
    end

    test "distinct on query with *",
      do:
        assert_equivalent(
          "SELECT COUNT(*) FROM (SELECT DISTINCT * FROM table) x",
          "SELECT COUNT(*) FROM (SELECT uid, numeric, string, bool FROM table GROUP BY 1, 2, 3, 4) x"
        )

    test "distinct with only aggregators",
      do:
        assert_equivalent(
          "SELECT COUNT(*) FROM (SELECT DISTINCT COUNT(*) + 1, ABS(AVG(numeric)) FROM table) x",
          "SELECT COUNT(*) FROM (SELECT COUNT(*) + 1, ABS(AVG(numeric)) FROM table) x"
        )

    test "rewrites subqueries in joins",
      do:
        assert_equivalent(
          "SELECT * FROM (SELECT count(*) as c FROM table) x JOIN (SELECT DISTINCT COUNT(*) + 1 as c, ABS(AVG(numeric)) FROM table) y ON x.c = y.c",
          "SELECT * FROM (SELECT count(*) as c FROM table) x JOIN (SELECT COUNT(*) + 1 as c, ABS(AVG(numeric)) FROM table) y ON x.c = y.c"
        )

    test "Drops DISTINCT and superfluous GROUP BYs when no aggregate and all selected columns are GROUPED BY" do
      assert_equivalent(
        "SELECT count(*) FROM (SELECT DISTINCT uid, numeric FROM table GROUP BY uid, numeric, string) x",
        "SELECT count(*) FROM (SELECT uid, numeric FROM table GROUP BY 1, 2) x"
      )
    end

    test "Drops DISTINCT when there is an aggregate if there is no extra GROUP BY" do
      assert_equivalent(
        "SELECT count(*) FROM (SELECT DISTINCT uid, count(*) FROM table GROUP BY 1) x",
        "SELECT count(*) FROM (SELECT uid, count(*) FROM table GROUP BY 1) x"
      )
    end
  end

  describe "normalizing ORDER BY" do
    test "normalizing unordered queries" do
      assert %{from: {:subquery, %{ast: %{order_by: []}}}} =
               compile!(
                 "SELECT STDDEV(uid) FROM (SELECT 'constant' FROM table) x",
                 postgres_data_source()
               )
    end

    test "removing constant ORDER BY clauses" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 "SELECT STDDEV(uid) FROM (SELECT 'constant' FROM table ORDER BY 1, uid) x",
                 postgres_data_source()
               )
    end

    test "ordering by uid if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 "SELECT STDDEV(uid) FROM (SELECT 'constant' FROM table ORDER BY 1) x",
                 postgres_data_source()
               )
    end

    test "ordering by uid from a join if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 """
                   SELECT STDDEV(uid) FROM (
                     SELECT 'constant' FROM table AS t1 JOIN table AS t2 ON t1.uid = t2.uid ORDER BY 1
                   ) x
                 """,
                 postgres_data_source()
               )
    end

    test "ordering by uid from a subquery if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 """
                   SELECT STDDEV(uid) FROM (
                     SELECT 'constant' FROM (SELECT uid FROM table) x ORDER BY 1
                   ) x
                 """,
                 postgres_data_source()
               )
    end
  end

  describe "making boolean comparisons explicit" do
    test "positive" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool",
        "SELECT STDDEV(uid) FROM table WHERE bool = true"
      )
    end

    test "negative" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE not bool",
        "SELECT STDDEV(uid) FROM table WHERE bool = false"
      )
    end

    test "multiple" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool and not cast(numeric as boolean)",
        "SELECT STDDEV(uid) FROM table WHERE bool = true and cast(numeric as boolean) = false"
      )
    end
  end

  describe "normalize boolean comparisons" do
    test "(not bool) = (not bool)" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE (not bool) = (not bool)",
        "SELECT STDDEV(uid) FROM table WHERE bool = bool"
      )
    end

    test "(not bool) = constant" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE (not bool) = true",
        "SELECT STDDEV(uid) FROM table WHERE bool = false"
      )
    end

    test "bool = (not bool)" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool = (not bool)",
        "SELECT STDDEV(uid) FROM table WHERE bool <> bool"
      )
    end

    test "bool <> constant" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool <> false and bool <> true",
        "SELECT STDDEV(uid) FROM table WHERE bool = true and bool = false"
      )
    end
  end

  describe "normalize filter clauses" do
    test "true <=> empty" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE true",
        "SELECT STDDEV(uid) FROM table"
      )
    end

    test "true = true <=> false = false" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table HAVING true = true",
        "SELECT STDDEV(uid) FROM table HAVING false = false"
      )
    end

    test "null <=> false" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE false",
        "SELECT STDDEV(uid) FROM table WHERE NULL"
      )
    end

    test "false = true <=> null = false" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table HAVING false = true",
        "SELECT STDDEV(uid) FROM table HAVING NULL = false"
      )
    end

    test "date constants comparison" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE date '2020-02-18' < date '2099-01-01'",
        "SELECT STDDEV(uid) FROM table"
      )
    end
  end

  test "join normalization" do
    assert_equivalent(
      "SELECT * FROM (SELECT STDDEV(uid) FROM table) AS t1 JOIN (SELECT STDDEV(uid) FROM table) AS t2 ON true",
      "SELECT * FROM (SELECT STDDEV(uid) FROM table) AS t1, (SELECT STDDEV(uid) FROM table) AS t2"
    )
  end

  describe "normalize boolean expressions" do
    test "x and true" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool and true",
        "SELECT STDDEV(uid) FROM table WHERE bool"
      )
    end

    test "true and x" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE true and bool",
        "SELECT STDDEV(uid) FROM table WHERE bool"
      )
    end

    test "false and x" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE false and bool",
        "SELECT STDDEV(uid) FROM table WHERE false"
      )
    end

    test "x and false" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool and false",
        "SELECT STDDEV(uid) FROM table WHERE false"
      )
    end

    test "x or true" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool or true",
        "SELECT STDDEV(uid) FROM table"
      )
    end

    test "true or x" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE true or bool",
        "SELECT STDDEV(uid) FROM table"
      )
    end

    test "false or x" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE false or bool",
        "SELECT STDDEV(uid) FROM table WHERE bool"
      )
    end

    test "x or false" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE bool or false",
        "SELECT STDDEV(uid) FROM table WHERE bool"
      )
    end

    test "multiple" do
      assert_equivalent(
        "SELECT STDDEV(uid) FROM table WHERE true and bool or false",
        "SELECT STDDEV(uid) FROM table WHERE bool"
      )
    end
  end

  test "not in multiple values" do
    assert_equivalent(
      "SELECT STDDEV(uid) FROM table WHERE numeric NOT IN (1, 2)",
      "SELECT STDDEV(uid) FROM table WHERE numeric <> 1 AND numeric <> 2"
    )
  end

  test "not in single value" do
    assert_equivalent(
      "SELECT STDDEV(uid) FROM table WHERE numeric NOT IN (1)",
      "SELECT STDDEV(uid) FROM table WHERE numeric <> 1"
    )
  end

  test "redundant case" do
    assert_equivalent(
      "SELECT CASE WHEN c > 0 THEN c ELSE c END AS c FROM (SELECT COUNT(*) AS c FROM table) t",
      "SELECT c FROM (SELECT COUNT(*) AS c FROM table) t"
    )
  end

  test "true branches in `case` statement" do
    assert_equivalent(
      "SELECT CASE WHEN numeric = 3 THEN 1 WHEN 2 = 2 THEN 0 ELSE NULL END FROM table",
      "SELECT CASE WHEN numeric = 3 THEN 1 ELSE 0 END FROM table"
    )
  end

  test "false branches in `case` statement" do
    assert_equivalent(
      "SELECT CASE WHEN 1 = 2 THEN 0 WHEN numeric = 3 THEN 1 ELSE NULL END FROM table",
      "SELECT CASE WHEN numeric = 3 THEN 1 ELSE NULL END FROM table"
    )
  end

  test "cast(real to integer)" do
    assert_equivalent(
      "SELECT CAST(numeric AS integer) AS x FROM table",
      "SELECT round(numeric) AS x FROM table"
    )
  end

  defp postgres_data_source(), do: %{data_source() | driver: Cloak.DataSource.PostgreSQL}

  defp data_source() do
    %{
      name: "normalization_data_source",
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
              Table.column("string", :text),
              Table.column("bool", :boolean)
            ],
            keys: %{"key" => :unknown}
          )
      }
    }
  end
end
