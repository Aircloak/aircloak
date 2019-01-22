defmodule Cloak.MaterializedViewTest do
  use ExUnit.Case, async: true

  alias Cloak.MaterializedView
  doctest MaterializedView

  setup_all do
    Cloak.Test.DB.create_table("mv1", "x integer, y text")
    Cloak.Test.DB.create_table("mv2", "x integer, y text", db_name: "db_name_mv2")
  end

  describe "compiled sql - " do
    test "simple materialized view" do
      view = view!(1, "select user_id, x from mv1")

      assert db_select(view) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "x" FROM "cloak_test"."mv1" AS "mv1"/
    end

    test "function support" do
      view = view!(1, "select user_id, hash(y) from mv1")

      assert db_select(view) ==
               ~s/SELECT "mv1"."user_id" AS "user_id",SUBSTR(MD5("mv1"."y"::text), 5, 8) AS "hash" / <>
                 ~s/FROM "cloak_test"."mv1" AS "mv1"/
    end

    test "subquery support" do
      view = view!(1, "select user_id, x from (select * from mv1) sq")

      assert db_select(view) ==
               ~s/SELECT "sq"."user_id" AS "user_id","sq"."x" AS "x" FROM / <>
                 ~s/(SELECT "mv1"."user_id" AS "user_id","mv1"."x" AS "x" FROM "cloak_test"."mv1" AS "mv1") AS "sq"/
    end

    test "join support" do
      view = view!(1, "select mv1.user_id, mv2.x from mv1 inner join mv2 on mv1.user_id = mv2.user_id")

      assert db_select(view) ==
               ~s/SELECT "mv1"."user_id" AS "user_id","mv2"."x" AS "x" / <>
                 ~s/FROM  "cloak_test"."mv1" AS "mv1" / <>
                 ~s/INNER JOIN "cloak_test"."db_name_mv2" AS "mv2" ON "mv1"."user_id" = "mv2"."user_id" /
    end

    test "tables are referenced by the user name" do
      view = view!(1, "select user_id, x from mv2")

      assert db_select(view) ==
               ~s/SELECT "mv2"."user_id" AS "user_id","mv2"."x" AS "x" FROM "cloak_test"."db_name_mv2" AS "mv2"/
    end

    test "tables can't be referenced by the db name" do
      assert {:error, error} = view(1, "select user_id, x from db_name_mv2")
      assert error == "Table `db_name_mv2` doesn't exist."
    end

    test "parsing error is reported" do
      assert {:error, error} = view(1, "select")
      assert error == "Expected `column definition` at line 1, column 7."
    end

    test "compilation error is reported" do
      assert {:error, error} = view(1, "select sqrt(y) from mv1")
      assert error == "Function `sqrt` requires arguments of type (`integer` | `real`), but got (`text`)."
    end

    test "anonymized subqueries can't be materialized" do
      assert {:error, error} = view(1, "select x from mv1")
      assert error == "At least one user id column must be selected."
    end

    test "emulated subqueries can't be materialized" do
      assert {:error, error} = view(1, "select user_id, dec_b64(y) from mv1")
      assert error == "Emulated query can't be materialized."
    end

    test "materialized view can't be created if the driver doesn't support it" do
      mongo_data_source = Map.put(data_source(), :driver, Cloak.DataSource.MongoDB)
      assert {:error, error} = MaterializedView.new(1, "select * from mv1", mongo_data_source)
      assert error == "This data source doesn't support materialized views."
    end
  end

  describe "name - " do
    test "different id leads to a different name" do
      view1 = view!(1, "select user_id, x from mv1")
      view2 = view!(2, "select user_id, x from mv1")
      refute MaterializedView.name(view1) == MaterializedView.name(view2)
    end

    test "different query leads to a different name" do
      view1 = view!(1, "select user_id, x from mv1")
      view2 = view!(1, "select user_id, y from mv1")
      refute MaterializedView.name(view1) == MaterializedView.name(view2)
    end
  end

  defp view!(id, statement) do
    {:ok, view} = view(id, statement)
    view
  end

  defp view(id, statement), do: MaterializedView.new(id, statement, data_source())

  defp data_source(), do: hd(Cloak.DataSource.all())

  defp db_select(view), do: Cloak.DataSource.PostgreSQL.db_query(view.query)
end
