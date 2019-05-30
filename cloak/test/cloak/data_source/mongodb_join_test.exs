defmodule Cloak.DataSource.MongoDBJoinTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.MongoDB

  import Cloak.Test.MongoHelpers

  @moduletag :exclude_in_dev
  @moduletag :mongodb

  setup_all do
    parameters = [hostname: "localhost", database: "cloaktest"]
    {:ok, conn} = Mongo.start_link(parameters)
    Mongo.delete_many(conn, "left", %{})
    Mongo.delete_many(conn, "right", %{})

    for i <- 1..20 do
      Mongo.insert_one!(conn, "left", %{id: i, name: "user#{i}", age: 30})
      Mongo.insert_one!(conn, "right", %{id: i, salary: rem(i, 3) * 100})
    end

    for i <- 21..25 do
      Mongo.insert_one!(conn, "left", %{id: i})
    end

    tables = %{
      "left" => Cloak.DataSource.Table.new("left", "id", db_name: "left"),
      "right" => Cloak.DataSource.Table.new("right", "id", db_name: "right")
    }

    data_source =
      %{
        name: "mongo_db_join",
        concurrency: 0,
        lcf_buckets_aggregation_limit: nil,
        driver: MongoDB,
        driver_info: MongoDB.driver_info(conn),
        parameters: parameters,
        tables: tables,
        errors: []
      }
      |> Cloak.DataSource.Table.load(conn)

    GenServer.stop(conn, :normal, :timer.seconds(5))
    {:ok, data_source: data_source}
  end

  test "inner join with tables in top-query", context do
    assert_query(
      context,
      """
        SELECT round(MEDIAN(salary)) FROM "left" INNER JOIN "right" ON "left".id = "right".id WHERE age = 30
      """,
      %{rows: [%{occurrences: 1, row: [68]}]}
    )
  end

  test "left join with table and sub-query in top-query", context do
    assert_query(
      context,
      """
        SELECT age FROM "left" LEFT JOIN (SELECT id AS rid FROM "right") AS t ON id = rid
      """,
      %{rows: [%{occurrences: 20, row: [30]}, %{occurrences: 5, row: [nil]}]}
    )
  end

  test "join in top-query with emulated where", context do
    assert_query(
      context,
      """
        SELECT count(age) FROM "left" INNER JOIN "right" ON "left".id = "right".id WHERE abs(salary) = 100
      """,
      %{rows: [%{occurrences: 1, row: [7]}]}
    )
  end

  test "left join with table and filtered sub-query in filtered top-query", context do
    assert_query(
      context,
      """
        SELECT age FROM "left" LEFT JOIN
        (SELECT id AS rid, abs(salary) AS s FROM "right" WHERE s = 100) AS t
        ON id = rid WHERE s = 100
      """,
      %{rows: [%{occurrences: 7, row: [30]}]}
    )
  end

  test "left join with tables in sub-query", context do
    assert_query(
      context,
      """
        SELECT COUNT(name) FROM
        (SELECT "left".id, name, salary as salary FROM "left" LEFT JOIN "right" ON "left".id = "right".id) AS t
        WHERE salary IN (100, 200)
      """,
      %{rows: [%{occurrences: 1, row: [14]}]}
    )
  end

  test "function in inner join condition", context do
    assert_query(
      context,
      """
        SELECT round(MEDIAN(salary))
        FROM "left" INNER JOIN "right"
        ON "left".id = "right".id AND age + 1 = 31
      """,
      %{rows: [%{occurrences: 1, row: [68]}]}
    )
  end

  test "complex function in inner join condition", context do
    assert_query(
      context,
      """
        SELECT AVG(salary)
        FROM "left" INNER JOIN "right" ON "left".id = "right".id AND abs(salary) = 100
      """,
      %{rows: [%{occurrences: 1, row: [100.0]}]}
    )
  end

  test "inner join with table and sub-query in sub-query", context do
    assert_query(
      context,
      """
        SELECT a FROM
          (SELECT id, age AS a FROM "left" INNER JOIN
            (SELECT id AS rid FROM "right") AS t ON rid = id) AS t
        WHERE a = 30
      """,
      %{rows: [%{occurrences: 20, row: [30]}]}
    )
  end

  test "inner join with two sub-queries", context do
    assert_query(
      context,
      """
        SELECT SUM(c) FROM
          (SELECT distinct id as lid FROM "left" WHERE age = 30) AS t1
          INNER JOIN
          (SELECT id AS rid, count(*) AS c FROM "right" GROUP BY id) AS t2
          ON rid = lid
      """,
      %{rows: [%{occurrences: 1, row: [20]}]}
    )
  end

  test "sample from join", context do
    assert_query(
      context,
      """
        SELECT sum(age) FROM "left" JOIN (SELECT id AS rid FROM "right") AS t ON id = rid SAMPLE_USERS 50%
      """,
      %{rows: [%{occurrences: 1, row: [300.0]}]}
    )
  end

  test "join with aliased tables", context do
    assert_query(
      context,
      """
        SELECT count(*) FROM "left" AS l JOIN "right" AS r ON l.id = r.id
      """,
      %{rows: [%{occurrences: 1, row: [20]}]}
    )
  end

  test "join with like filter", context do
    assert_query(
      context,
      """
        SELECT round(MEDIAN(salary)) FROM "left"
          INNER JOIN "right" ON "left".id = "right".id AND name LIKE 'user%'
      """,
      %{rows: [%{occurrences: 1, row: [77]}]}
    )
  end

  test "triple join", context do
    assert_query(
      context,
      """
        SELECT count(*) FROM "left"
          JOIN (SELECT id AS rid FROM "right") AS t ON "left".id = rid
          JOIN "right" ON rid = "right".id
      """,
      %{rows: [%{occurrences: 1, row: [20]}]}
    )
  end

  test "right join", context do
    assert_query(
      context,
      """
        SELECT age FROM "right" RIGHT JOIN "left" ON "left".id = "right".id
      """,
      %{rows: [%{occurrences: 20, row: [30]}, %{occurrences: 5, row: [nil]}]}
    )
  end
end
