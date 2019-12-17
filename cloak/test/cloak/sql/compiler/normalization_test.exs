defmodule Cloak.Sql.Compiler.Normalization.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Expression
  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  defmacrop assert_equivalent(query, alternative) do
    quote bind_quoted: [query: query, alternative: alternative] do
      assert compile!(query, data_source()) == compile!(alternative, data_source())
    end
  end

  defmacrop refute_equivalent(query, alternative) do
    quote bind_quoted: [query: query, alternative: alternative] do
      refute compile!(query, data_source()) == compile!(alternative, data_source())
    end
  end

  describe "constant normalization" do
    test "normalizing constant expressions" do
      result1 = compile!("SELECT * FROM table WHERE numeric = 2 * 3 + 4", data_source())
      result2 = compile!("SELECT * FROM table WHERE numeric = 10", data_source())

      assert result1.where == result2.where
    end

    test "normalizing constant expressions with NULL" do
      result1 = compile!("SELECT * FROM table WHERE numeric = 2 + NULL", data_source())
      result2 = compile!("SELECT * FROM table WHERE numeric = NULL", data_source())

      assert result1.where == result2.where
    end

    test "[Issue #3384] normalizing constant bucket" do
      result1 = compile!("SELECT bucket(2 BY 7) FROM table", data_source())
      result2 = compile!("SELECT 0::real AS \"bucket\" FROM table", data_source())

      assert result1 == result2
    end

    test "[Issue #3413] normalizing aggregators over constants" do
      result1 = compile!("SELECT ABS(SUM(1 - 10)) FROM table", data_source())
      result2 = compile!("SELECT ABS(SUM(-9)) FROM table", data_source())

      assert result1 == result2
    end
  end

  test "normalization in subqueries" do
    %{from: {:subquery, %{ast: result1}}} =
      compile!("SELECT * FROM (SELECT * FROM table WHERE numeric = 2 * 3 + 4) x", data_source())

    %{from: {:subquery, %{ast: result2}}} =
      compile!("SELECT * FROM (SELECT * FROM table WHERE numeric = 10) x", data_source())

    assert result1.where == result2.where
  end

  test "normalizing trivial like patterns into =" do
    result1 = compile!("SELECT * FROM table WHERE string LIKE 'abc'", data_source())
    result2 = compile!("SELECT * FROM table WHERE string = 'abc'", data_source())

    assert result1.where == result2.where
  end

  test "normalizing trivial not like patterns into <>" do
    result1 = compile!("SELECT * FROM table WHERE string NOT LIKE 'abc'", data_source())
    result2 = compile!("SELECT * FROM table WHERE string <> 'abc'", data_source())

    assert result1.where == result2.where
  end

  test "normalizing trivial ilike patterns into =" do
    result1 = compile!("SELECT * FROM table WHERE string ILIKE 'Abc'", data_source())
    result2 = compile!("SELECT * FROM table WHERE lower(string) = 'abc'", data_source())

    assert scrub_locations(result1).where == scrub_locations(result2).where
  end

  test "normalizing trivial not ilike patterns <>" do
    result1 = compile!("SELECT * FROM table WHERE string NOT ILIKE 'Abc'", data_source())
    result2 = compile!("SELECT * FROM table WHERE lower(string) <> 'abc'", data_source())

    assert scrub_locations(result1).where == scrub_locations(result2).where
  end

  describe "remove noops" do
    test "a cast of integer to integer" do
      assert_equivalent(
        "SELECT * FROM table WHERE cast(numeric AS integer) = 1",
        "SELECT * FROM table WHERE numeric = 1"
      )
    end

    for function <- ~w/round trunc/ do
      test "#{function} of integer without precision is removed" do
        assert_equivalent(
          "SELECT * FROM table WHERE #{unquote(function)}(numeric) = 1",
          "SELECT * FROM table WHERE numeric = 1"
        )
      end

      test "#{function} of integer with precision isn't removed" do
        refute_equivalent(
          "SELECT * FROM table WHERE #{unquote(function)}(numeric, 0) = 1",
          "SELECT * FROM table WHERE numeric = 1"
        )
      end
    end

    for function <- ~w/ceil ceiling floor/ do
      test "#{function} of integer is removed" do
        assert_equivalent(
          "SELECT * FROM table WHERE #{unquote(function)}(numeric) = 1",
          "SELECT * FROM table WHERE numeric = 1"
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

      assert error =~ ~r/Column `numeric` .* needs to appear in the `GROUP BY` clause/
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

      assert error =~ ~r/Column `numeric` .* needs to appear in the `GROUP BY` clause/
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
                 sql_server_data_source()
               )
    end

    test "removing constant ORDER BY clauses" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 "SELECT STDDEV(uid) FROM (SELECT 'constant' FROM table ORDER BY 1, uid) x",
                 sql_server_data_source()
               )
    end

    test "ordering by uid if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 "SELECT STDDEV(uid) FROM (SELECT 'constant' FROM table ORDER BY 1) x",
                 sql_server_data_source()
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
                 sql_server_data_source()
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
                 sql_server_data_source()
               )
    end
  end

  test "stripping source locations" do
    result1 = compile!("SELECT    count(*)    FROM table WHERE   abs(numeric) = 1", data_source())
    result2 = compile!("SELECT count(*) FROM table WHERE abs(numeric) = 1", data_source())

    assert result1 == result2
  end

  describe "making boolean comparisons explicit" do
    test "positive" do
      result1 = compile!("SELECT * FROM table WHERE bool", data_source())
      result2 = compile!("SELECT * FROM table WHERE bool = true", data_source())

      assert result1.where == result2.where
    end

    test "negative" do
      result1 = compile!("SELECT * FROM table WHERE not bool", data_source())
      result2 = compile!("SELECT * FROM table WHERE bool = false", data_source())

      assert result1.where == result2.where
    end

    test "multiple" do
      result1 = compile!("SELECT * FROM table WHERE bool and not cast(numeric as boolean)", data_source())
      result2 = compile!("SELECT * FROM table WHERE bool = true and cast(numeric as boolean) = false", data_source())

      assert result1.where == result2.where
    end
  end

  defp sql_server_data_source(), do: %{data_source() | driver: Cloak.DataSource.SQLServer}

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
