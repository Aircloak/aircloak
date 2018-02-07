defmodule Cloak.Sql.Compiler.Normalization.Test do
  use ExUnit.Case, async: true

  alias Cloak.Sql.Expression
  alias Cloak.DataSource.Table

  import Cloak.Test.QueryHelpers

  test "normalizing constant expressions" do
    result1 = compile!("SELECT * FROM table WHERE numeric = 2 * 3 + 4", data_source())
    result2 = compile!("SELECT * FROM table WHERE numeric = 10", data_source())

    assert result1.where == result2.where
  end

  test "normalization in subqueries" do
    %{from: {:subquery, %{ast: result1}}} = compile!(
      "SELECT * FROM (SELECT * FROM table WHERE numeric = 2 * 3 + 4) x", data_source())
    %{from: {:subquery, %{ast: result2}}} = compile!(
      "SELECT * FROM (SELECT * FROM table WHERE numeric = 10) x", data_source())

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

  describe "normalizing ORDER BY" do
    test "normalizing unordered queries" do
      assert %{from: {:subquery, %{ast: %{order_by: []}}}} =
        compile!("SELECT COUNT(*) FROM (SELECT 'constant' FROM table) x", sql_server_data_source())
    end

    test "removing constant ORDER BY clauses" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _}]}}}} =
        compile!("SELECT COUNT(*) FROM (SELECT 'constant' FROM table ORDER BY 1, uid) x", sql_server_data_source())
    end

    test "ordering by uid if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _}]}}}} =
        compile!("SELECT COUNT(*) FROM (SELECT 'constant' FROM table ORDER BY 1) x", sql_server_data_source())
    end

    test "ordering by uid from a join if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _}]}}}} = compile!("""
        SELECT COUNT(*) FROM (
          SELECT 'constant' FROM table AS t1 JOIN table AS t2 ON t1.uid = t2.uid ORDER BY 1
        ) x
      """, sql_server_data_source())
    end

    test "ordering by uid from a subquery if all clauses are removed" do
      assert %{from: {:subquery, %{ast: %{order_by: [{%Expression{name: "uid"}, _}]}}}} = compile!("""
        SELECT COUNT(*) FROM (
          SELECT 'constant' FROM (SELECT uid FROM table) x ORDER BY 1
        ) x
      """, sql_server_data_source())
    end
  end

  describe "remove_noops" do
    test "a cast of integer to integer" do
      result1 = remove_noops!("SELECT * FROM table WHERE cast(numeric AS integer) = 1", data_source())
      result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

      assert scrub_locations(result1).where == scrub_locations(result2).where
    end

    for function <- ~w/round trunc/ do
      test "#{function} of integer without precision is removed" do
        result1 = remove_noops!("SELECT * FROM table WHERE #{unquote(function)}(numeric) = 1", data_source())
        result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

        assert scrub_locations(result1).where == scrub_locations(result2).where
      end

      test "#{function} of integer with precision isn't removed" do
        result1 = remove_noops!("SELECT * FROM table WHERE #{unquote(function)}(numeric, 0) = 1", data_source())
        result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

        refute scrub_locations(result1).where == scrub_locations(result2).where
      end
    end

    for function <- ~w/ceil ceiling floor/ do
      test "#{function} of integer is removed" do
        result1 = remove_noops!("SELECT * FROM table WHERE #{unquote(function)}(numeric) = 1", data_source())
        result2 = remove_noops!("SELECT * FROM table WHERE numeric = 1", data_source())

        assert scrub_locations(result1).where == scrub_locations(result2).where
      end
    end
  end

  test "stripping source locations" do
    result1 = compile!("SELECT    count(*)    FROM table WHERE   abs(numeric) = 1", data_source())
    result2 = compile!("SELECT count(*) FROM table WHERE abs(numeric) = 1", data_source())

    assert result1 == result2
  end

  defp remove_noops!(query, data_source, parameters \\ [], views \\ %{}) do
    {:ok, parsed} = Cloak.Sql.Parser.parse(query)

    parsed
    |> Cloak.Sql.Compiler.ASTNormalization.normalize()
    |> Cloak.Sql.Compiler.Specification.compile(data_source, parameters, views)
    |> Cloak.Sql.Compiler.Validation.verify_query()
    |> Cloak.Sql.Compiler.Normalization.remove_noops()
  end

  defp sql_server_data_source(), do: %{data_source() | driver: Cloak.DataSource.SQLServer}

  defp data_source() do
    %{
      driver: Cloak.DataSource.PostgreSQL,
      tables: %{
        table: Cloak.DataSource.Table.new("table", "uid",
          db_name: "table",
          columns: [
            Table.column("uid", :integer),
            Table.column("numeric", :integer),
            Table.column("string", :text),
          ],
          keys: ["key"]
        ),
      }
    }
  end
end
