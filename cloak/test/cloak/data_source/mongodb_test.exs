defmodule Cloak.DataSource.MongoDBTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.{Table, MongoDB}

  import Cloak.Test.MongoHelpers

  @moduletag :exclude_in_dev
  @moduletag :mongodb
  @table "test"

  setup do
    parameters = [hostname: "localhost", database: "cloaktest"]
    {:ok, conn} = Mongo.start_link(parameters)
    Mongo.delete_many(conn, @table, %{})
    for i <- 1..10 do
      value = %{name: "user#{i}", age: 30, male: true, date: %BSON.DateTime{utc: 1_437_940_203_000},
        bills: [%{issuer: "vendor", ids: ["1", "2"]}]}
      Mongo.insert_one!(conn, @table, value)
    end
    Mongo.insert_one!(conn, @table, %{name: nil, male: nil, mixed: true, bills: nil})
    Mongo.insert_one!(conn, @table, %{mixed: "dummy"})
    Mongo.insert_one!(conn, @table, %{mixed: [1, 2, 3]})
    Mongo.insert_one!(conn, @table, %{mixed: %{a: 1, b: 2}})
    for _i <- 11..15 do
      Mongo.insert_one!(conn, @table, %{})
    end
    decoder = %{method: "text_to_real", columns: ["bills.ids"]}
    table_config = Table.new(@table, "_id", db_name: @table, decoders: [decoder])
    tables =
      conn
      |> MongoDB.load_tables(table_config)
      |> Enum.map(&Cloak.Query.DataDecoder.init/1)
      |> Enum.map(&{&1.name, &1})
      |> Enum.into(%{})
    GenServer.stop(conn)

    data_source = %{
      name: "mongo_db_standard",
      driver: MongoDB,
      parameters: parameters,
      tables: tables
    }
    {:ok, data_source: data_source}
  end

  test "schema mapping", context do
    %{@table => root, @table <> "_bills" => bills, @table <> "_bills_ids" => ids} = context.data_source.tables
    assert root.columns == [
      Table.column("_id", :text),
      Table.column("age", :real),
      Table.column("bills#", :integer),
      Table.column("date", :datetime),
      Table.column("male", :boolean),
      Table.column("mixed", :unknown),
      Table.column("name", :text),
    ]
    assert bills.columns == [
      Table.column("_id", :text),
      Table.column("age", :real),
      Table.column("date", :datetime),
      Table.column("male", :boolean),
      Table.column("mixed", :unknown),
      Table.column("name", :text),
      Table.column("bills.ids#", :integer),
      Table.column("bills.issuer", :text),
    ]
    assert ids.columns == [
      Table.column("_id", :text),
      Table.column("age", :real),
      Table.column("date", :datetime),
      Table.column("male", :boolean),
      Table.column("mixed", :unknown),
      Table.column("name", :text),
      Table.column("bills.issuer", :text),
      Table.column("bills.ids", :real),
    ]
  end

  test "basic queries on root table", context do
    assert_query context, "SELECT COUNT(name) FROM #{@table}", %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query context, "SELECT AVG(age) FROM #{@table}", %{rows: [%{occurrences: 1, row: [30.0]}]}
    assert_query context, "SELECT DISTINCT male FROM #{@table} WHERE male IS NOT NULL",
      %{rows: [%{occurrences: 1, row: [true]}]}
  end

  test "aliased query", context, do:
    assert_query context, "SELECT COUNT(a.name) FROM #{@table} a", %{rows: [%{occurrences: 1, row: [10]}]}

  test "conditions on root table", context do
    assert_query context, "SELECT COUNT(name) FROM #{@table} WHERE male = false",
      %{rows: [%{occurrences: 1, row: [0]}]}
    assert_query context, "SELECT COUNT(name) FROM #{@table} WHERE false <> male",
      %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query context, "SELECT COUNT(name) FROM #{@table} WHERE 30 = age",
      %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "virtual tables", context do
    assert_query context, "SELECT COUNT(name) FROM #{@table}_bills", %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query context, "SELECT COUNT(name) FROM #{@table}_bills_ids", %{rows: [%{occurrences: 1, row: [20]}]}
  end

  test "virtual columns", context do
    assert_query context, "SELECT AVG(bills#) FROM #{@table} WHERE bills# = 1", %{rows: [%{occurrences: 1, row: [1.0]}]}
    assert_query context, "SELECT AVG(bills.ids#) FROM #{@table}_bills", %{rows: [%{occurrences: 1, row: [2.0]}]}
  end

  test "complex queries on virtual columns and tables", context do
    assert_query context,
      "SELECT COUNT(*) FROM #{@table}_bills WHERE bills.issuer = 'vendor' AND male = TRUE AND abs(bills.ids#) + 1 = 3",
      %{rows: [%{occurrences: 1, row: [10]}]}

    assert_query context,
      "SELECT bills.issuer, COUNT(*) FROM #{@table}_bills_ids WHERE bills.ids = 1 GROUP BY bills.issuer",
      %{rows: [%{occurrences: 1, row: ["vendor", 10]}]}

    assert_query context,
      """
        SELECT AVG(bills#) FROM
          (SELECT _id, bills# FROM #{@table} WHERE abs(bills#) = 1 AND bills# = 1) AS t
        WHERE abs(bills#) = 1 AND bills# = 1
      """, %{rows: [%{occurrences: 1, row: [1.0]}]}
  end

  test "basic sub-queries", context do
    assert_query context, "SELECT AVG(age) FROM (SELECT _id, age FROM #{@table}) AS t",
      %{rows: [%{occurrences: 1, row: [30.0]}]}
    assert_query context, "SELECT COUNT(name) FROM (SELECT _id, name FROM #{@table}_bills) AS t",
      %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "sub-queries with limit/offset", context do
    assert_query context, """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@table}_bills_ids ORDER BY _id LIMIT 10) AS t
      """, %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query context, """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@table}_bills_ids ORDER BY _id LIMIT 10 OFFSET 10) AS t
      """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "sub-queries with non-selected order by", context do
    assert_query context, """
        SELECT COUNT(name) FROM (SELECT _id, name FROM #{@table}_bills_ids ORDER BY age LIMIT 10) AS t
      """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "functions in sub-queries", context do
    assert_query context, "SELECT AVG(age) FROM (SELECT _id, age * 2 + 1 AS age FROM #{@table}) AS t",
      %{rows: [%{occurrences: 1, row: [61.0]}]}
    assert_query context, "SELECT name FROM (SELECT _id, lower(left(name, 4)) AS name FROM #{@table}) AS t",
      %{rows: [%{occurrences: 9, row: [nil]}, %{occurrences: 10, row: ["user"]}]}
  end

  test "aggregation in sub-queries", context do
    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, COUNT(*) AS value FROM #{@table} GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [19, 19]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, COUNT(age) AS value FROM #{@table} GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [19, 10]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, AVG(age) AS value FROM #{@table} GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [19, 300.0]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM
        (SELECT _id, COUNT(DISTINCT bills.issuer) AS value FROM #{@table}_bills_ids GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [10, 10]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM
        (SELECT _id, SUM(DISTINCT bills.ids) AS value FROM #{@table}_bills_ids GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [10, 30.0]}]}
  end

  test "distinct in sub-queries", context do
    assert_query context, """
        SELECT COUNT(*) FROM (SELECT DISTINCT _id, length(bills.issuer) AS value FROM #{@table}_bills_ids) AS t
      """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "error on invalid conditions", context do
    assert_query context, "SELECT COUNT(name) FROM #{@table} WHERE true = true",
      %{error: "Conditions on MongoDB data sources have to be between a column and a constant."}
    assert_query context, "SELECT COUNT(name) FROM #{@table} WHERE age = abs(age)",
      %{error: "Conditions on MongoDB data sources have to be between a column and a constant."}
  end

  test "datetime support", context do
    assert_query context, "SELECT DISTINCT date FROM #{@table} WHERE date IS NOT NULL",
      %{rows: [%{occurrences: 1, row: ["2015-07-26T19:50:03.000000"]}]}
    assert_query context, """
        SELECT max(year) FROM (SELECT _id, year(date) FROM #{@table} WHERE date = '2015-07-26 19:50:03') AS t
      """, %{rows: [%{occurrences: 1, row: [2015]}]}
  end

  test "datetime - quarter support", context do
    assert_query context, "SELECT quarter(date) FROM #{@table} WHERE date IS NOT NULL GROUP BY 1",
      %{rows: [%{occurrences: 1, row: [3]}]}
  end

  test "unsupported functions in sub-queries are emulated", context do
    assert_query context, "SELECT AVG(age) FROM (SELECT _id, round(age) AS age FROM #{@table}) AS t",
      %{rows: [%{occurrences: 1, row: [30.0]}]}
  end

  test "dotted names", context do
    assert_query context, """
        SELECT count(bills.issuer) FROM (SELECT _id, bills.issuer FROM #{@table}_bills_ids) AS t
      """, %{rows: [%{occurrences: 1, row: [20]}]}
  end

  test "functions in conditions on root table", context do
    assert_query context, "SELECT COUNT(name) FROM #{@table} WHERE 2 * abs(age) = 60",
      %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "functions in conditions on virtual table and column", context do
    assert_query context, "SELECT COUNT(name) FROM #{@table}_bills WHERE bills.ids# % 2 = 0",
      %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "functions in conditions in subqueries", context do
    assert_query context, """
      SELECT COUNT(name) FROM (SELECT _id, name FROM #{@table}_bills WHERE age / 3 = 10 AND abs(age + 2) <> 20) AS t
    """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "outer functions in having clauses in subqueries", context do
    assert_query context, """
      SELECT COUNT(*) FROM (SELECT _id, COUNT(name) FROM #{@table}_bills
      GROUP BY _id HAVING abs(COUNT(DISTINCT name)) - 1 = 0) AS t
    """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "inner functions in having clauses in subqueries", context do
    assert_query context, """
      SELECT COUNT(*) FROM (SELECT _id, COUNT(name) FROM #{@table}_bills
      GROUP BY _id HAVING COUNT(abs(age)) = 1) AS t
    """, %{rows: [%{occurrences: 1, row: [10]}]}
  end

  test "string length", context do
    assert_query context, "SELECT v FROM (SELECT _id, length(name) AS v FROM #{@table}) AS t",
      %{rows: [%{occurrences: 9, row: [5]}, %{occurrences: 9, row: [nil]}]}
  end

  test "integer division", context do
    assert_query context, "SELECT v FROM (SELECT _id, div(trunc(age), -7) AS v FROM #{@table}) AS t",
      %{rows: [%{occurrences: 10, row: [-4]}, %{occurrences: 9, row: [nil]}]}
  end

  test "select constant", context do
    assert_query context, "SELECT v FROM (SELECT _id, 1 + 1 AS v FROM #{@table}) AS t",
      %{rows: [%{occurrences: 19, row: [2]}]}
  end

  test "substring", context do
    assert_query context, """
        SELECT v FROM (SELECT _id, substring(name FROM 2 FOR 3) AS v FROM #{@table}) AS t WHERE v IS NOT NULL
      """, %{rows: [%{occurrences: 10, row: ["ser"]}]}
  end
end
