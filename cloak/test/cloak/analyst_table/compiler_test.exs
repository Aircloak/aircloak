defmodule Cloak.AnalystTable.CompilerTest do
  use ExUnit.Case, async: true

  alias Cloak.AnalystTable.Compiler

  @moduletag :analyst_tables

  setup_all do
    Cloak.Test.DB.create_table("mv1", "x integer, y text")
    Cloak.Test.DB.create_table("mv2", "x integer, y text", db_name: "db_name_mv2")
  end

  describe "compiled sql - " do
    test "simple analyst table" do
      {:ok, query} = compile("table_name", "select user_id, x from mv1")

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "x" FROM "cloak_test"."mv1" AS "mv1"/
    end

    test "function support" do
      {:ok, query} = compile("table_name", "select user_id, hash(y) from mv1")

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id",SUBSTR(MD5("mv1"."y"::text), 5, 8) AS "hash",/ <>
                 ~s/"mv1"."y" AS "__ac_nlc__0" FROM "cloak_test"."mv1" AS "mv1"/
    end

    test "subquery support" do
      {:ok, query} = compile("table_name", "select user_id, x from (select * from mv1) sq")

      assert db_select(query) ==
               ~s/SELECT "sq"."user_id" AS "user_id","sq"."x" AS "x" FROM / <>
                 ~s/(SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "x" FROM "cloak_test"."mv1" AS "mv1") AS "sq"/
    end

    test "join support" do
      {:ok, query} =
        compile(
          "table_name",
          "select mv1.user_id, mv2.x from mv1 inner join mv2 on mv1.user_id = mv2.user_id"
        )

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv2"."x" AS "x" / <>
                 ~s/FROM  "cloak_test"."mv1" AS "mv1" / <>
                 ~s/INNER JOIN "cloak_test"."db_name_mv2" AS "mv2" ON "mv1"."user_id" = "mv2"."user_id" /
    end

    test "tables are referenced by the user name" do
      {:ok, query} = compile("table_name", "select user_id, x from mv2")

      assert db_select(query) ==
               ~s/SELECT "mv2"."user_id" AS "user_id","mv2"."x" AS "x" FROM "cloak_test"."db_name_mv2" AS "mv2"/
    end

    test "tables can't be referenced by the db name" do
      assert {:error, error} = compile("table_name", "select user_id, x from db_name_mv2")
      assert error == "Table `db_name_mv2` doesn't exist."
    end

    test "parsing error is reported" do
      assert {:error, error} = compile("table_name", "select")
      assert error == "Expected `column definition` at line 1, column 7."
    end

    test "compilation error is reported" do
      assert {:error, error} = compile("table_name", "select sqrt(y) from mv1")
      assert error == "Function `sqrt` requires arguments of type (`integer` | `real`), but got (`text`)."
    end

    test "anonymized subqueries can't be materialized" do
      assert {:error, error} = compile("table_name", "select x from mv1")
      assert error == "At least one user id column must be selected."
    end

    test "emulated subqueries can't be materialized" do
      assert {:error, error} = compile("table_name", "select user_id, dec_b64(y) from mv1")
      assert error == "Emulated query can't be materialized."
    end

    test "analyst table can't be created if the table with the same name already exists" do
      assert {:error, error} = compile("mv1", "select user_id from mv1")
      assert error == "The table with the given name already exists."
    end

    test "analyst table can't be created if column names are duplicate" do
      assert {:error, error} = compile("table_name", "select user_id, x, y, x, y, x from mv1")
      assert error == "Duplicate column names: `x`, `y`"

      assert {:error, error} = compile("table_name", "select user_id, sqrt(x), sqrt(3) from mv1")
      assert error == "Duplicate column names: `sqrt`"

      assert {:error, error} = compile("table_name", "select * from mv1 inner join mv2 on mv1.user_id = mv2.user_id")
      assert error == "Duplicate column names: `user_id`, `x`, `y`"
    end

    test "ranges are aligned" do
      {:ok, query} = compile("table_name", "select user_id, x from mv1 where x between 1 and 99")

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "x" / <>
                 ~s/FROM "cloak_test"."mv1" AS "mv1" WHERE ("mv1"."x" >= 0.0) AND ("mv1"."x" < 100.0)/
    end

    test "unbounded ranges are not allowed" do
      assert {:error, error} = compile("table_name", "select user_id, x from mv1 where x > 10")
      assert error == "Column `x` from table `mv1` must be limited to a finite, nonempty range."
    end
  end

  describe "potential noise layer components" do
    test "from select list" do
      {:ok, query} = compile("table_name", "select user_id, x * x AS x from mv1")

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id",(CAST("mv1"."x" AS bigint)*CAST("mv1"."x" AS bigint)) AS / <>
                 ~s/"x","mv1"."x" AS "__ac_nlc__0" FROM "cloak_test"."mv1" AS "mv1"/
    end

    test "with a condition" do
      {:ok, query} = compile("table_name", "select user_id from mv1 WHERE x * x = 10")

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "__ac_nlc__0"/ <>
                 ~s/ FROM "cloak_test"."mv1" AS "mv1" WHERE (CAST("mv1"."x" AS bigint)*CAST("mv1"."x" AS bigint)) = 10/
    end

    test "with aggregation" do
      {:ok, query} = compile("table_name", "select user_id, x * x AS x FROM mv1 GROUP BY 1, 2")

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id",(CAST("mv1"."x" AS bigint)*CAST("mv1"."x" AS bigint)) AS "x",/ <>
                 ~s/MIN("mv1"."x") AS "__ac_nlc__0",/ <>
                 ~s/MAX("mv1"."x") AS "__ac_nlc__1",COUNT(*) AS "__ac_nlc__2"/ <>
                 ~s/ FROM "cloak_test"."mv1" AS "mv1"/ <>
                 ~s/ GROUP BY "mv1"."user_id", (CAST("mv1"."x" AS bigint)*CAST("mv1"."x" AS bigint))/
    end
  end

  test "selecting a secondary uid from a subquery" do
    assert {:ok, query} = compile("table_name", "select uid2 from (select user_id, user_id as uid2 from mv1) sq")
    assert query.type == :restricted
  end

  defp compile(table_name, statement, data_source \\ data_source()),
    do: Compiler.compile(table_name, statement, 1, data_source, nil, %{})

  defp data_source(), do: hd(Cloak.DataSource.all())

  defp db_select(query) do
    create_statement = Cloak.DataSource.Driver.SQL.AnalystTables.create_table_from_query("some_table", query)
    String.replace(create_statement, ~r/^CREATE TABLE "[^\s]*" AS /, "")
  end
end
