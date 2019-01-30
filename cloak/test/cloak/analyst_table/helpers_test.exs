defmodule Cloak.AnalystTable.HelpersTest do
  use ExUnit.Case, async: true

  alias Cloak.AnalystTable.Helpers

  @moduletag :analyst_tables

  setup_all do
    Cloak.Test.DB.create_table("mv1", "x integer, y text")
    Cloak.Test.DB.create_table("mv2", "x integer, y text", db_name: "db_name_mv2")
  end

  describe "compiled sql - " do
    test "simple analyst table" do
      {:ok, query} = Helpers.compile("table_name", "select user_id, x from mv1", data_source())

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "x" FROM "cloak_test"."mv1" AS "mv1"/
    end

    test "function support" do
      {:ok, query} = Helpers.compile("table_name", "select user_id, hash(y) from mv1", data_source())

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id",SUBSTR(MD5("mv1"."y"::text), 5, 8) AS "hash" / <>
                 ~s/FROM "cloak_test"."mv1" AS "mv1"/
    end

    test "subquery support" do
      {:ok, query} = Helpers.compile("table_name", "select user_id, x from (select * from mv1) sq", data_source())

      assert db_select(query) ==
               ~s/SELECT "sq"."user_id" AS "user_id","sq"."x" AS "x" FROM / <>
                 ~s/(SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "x" FROM "cloak_test"."mv1" AS "mv1") AS "sq"/
    end

    test "join support" do
      {:ok, query} =
        Helpers.compile(
          "table_name",
          "select mv1.user_id, mv2.x from mv1 inner join mv2 on mv1.user_id = mv2.user_id",
          data_source()
        )

      assert db_select(query) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv2"."x" AS "x" / <>
                 ~s/FROM  "cloak_test"."mv1" AS "mv1" / <>
                 ~s/INNER JOIN "cloak_test"."db_name_mv2" AS "mv2" ON "mv1"."user_id" = "mv2"."user_id" /
    end

    test "tables are referenced by the user name" do
      {:ok, query} = Helpers.compile("table_name", "select user_id, x from mv2", data_source())

      assert db_select(query) ==
               ~s/SELECT "mv2"."user_id" AS "user_id","mv2"."x" AS "x" FROM "cloak_test"."db_name_mv2" AS "mv2"/
    end

    test "tables can't be referenced by the db name" do
      assert {:error, error} = Helpers.compile("table_name", "select user_id, x from db_name_mv2", data_source())
      assert error == "Table `db_name_mv2` doesn't exist."
    end

    test "parsing error is reported" do
      assert {:error, error} = Helpers.compile("table_name", "select", data_source())
      assert error == "Expected `column definition` at line 1, column 7."
    end

    test "compilation error is reported" do
      assert {:error, error} = Helpers.compile("table_name", "select sqrt(y) from mv1", data_source())
      assert error == "Function `sqrt` requires arguments of type (`integer` | `real`), but got (`text`)."
    end

    test "anonymized subqueries can't be materialized" do
      assert {:error, error} = Helpers.compile("table_name", "select x from mv1", data_source())
      assert error == "At least one user id column must be selected."
    end

    test "emulated subqueries can't be materialized" do
      assert {:error, error} = Helpers.compile("table_name", "select user_id, dec_b64(y) from mv1", data_source())
      assert error == "Emulated query can't be materialized."
    end

    test "analyst table can't be created if the driver doesn't support it" do
      mongo_data_source = Map.put(data_source(), :driver, Cloak.DataSource.MongoDB)
      assert {:error, error} = Helpers.compile("table_name", "select * from mv1", mongo_data_source)
      assert error == "This data source doesn't support analyst tables."
    end

    test "analyst table can't be created if the table with the same name already exists" do
      assert {:error, error} = Helpers.compile("mv1", "select user_id from mv1", data_source())
      assert error == "The table with the given name already exists."
    end

    test "analyst table can't be created if column names are duplicate" do
      assert {:error, error} = Helpers.compile("table_name", "select user_id, x, y, x, y, x from mv1", data_source())
      assert error == "Duplicate column names: `x`, `y`"

      assert {:error, error} = Helpers.compile("table_name", "select user_id, sqrt(x), sqrt(3) from mv1", data_source())
      assert error == "Duplicate column names: `sqrt`"

      assert {:error, error} =
               Helpers.compile(
                 "table_name",
                 "select * from mv1 inner join mv2 on mv1.user_id = mv2.user_id",
                 data_source()
               )

      assert error == "Duplicate column names: `user_id`, `x`, `y`"
    end
  end

  defp data_source(), do: hd(Cloak.DataSource.all())

  defp db_select(query) do
    {create_statement, _table_name} = Cloak.DataSource.SqlBuilder.create_table_statement(1, query)
    String.replace(create_statement, ~r/^CREATE TABLE "[^\s]*" AS /, "")
  end
end
