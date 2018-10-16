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

  test "normalizing constant expressions" do
    result1 = compile!("SELECT * FROM table WHERE numeric = 2 * 3 + 4", data_source())
    result2 = compile!("SELECT * FROM table WHERE numeric = 10", data_source())

    assert result1.where == result2.where
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
          "SELECT COUNT(*) FROM (SELECT uid, numeric, string FROM table GROUP BY 1, 2, 3) x"
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
                 "SELECT COUNT(*) FROM (SELECT 'constant' FROM table) x",
                 sql_server_data_source()
               )
    end

    test "removing constant ORDER BY clauses" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 "SELECT COUNT(*) FROM (SELECT 'constant' FROM table ORDER BY 1, uid) x",
                 sql_server_data_source()
               )
    end

    test "ordering by uid if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 "SELECT COUNT(*) FROM (SELECT 'constant' FROM table ORDER BY 1) x",
                 sql_server_data_source()
               )
    end

    test "ordering by uid from a join if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _, _}]}}}} =
               compile!(
                 """
                   SELECT COUNT(*) FROM (
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
                   SELECT COUNT(*) FROM (
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
              Table.column("string", :text)
            ],
            keys: ["key"]
          )
      }
    }
  end
end
