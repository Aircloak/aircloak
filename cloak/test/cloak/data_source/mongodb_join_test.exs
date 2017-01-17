defmodule Cloak.DataSource.MongoDBJoinTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.MongoDB
  alias Cloak.Query.Runner

  @moduletag :exclude_in_dev
  @moduletag :mongodb

  defmacro assert_query(context, query, parameters \\ [], expected_response) do
    quote do
      Runner.start("1", unquote(context).data_source, unquote(query), unquote(parameters), %{}, {:process, self()})
      response = receive do
        {:reply, response} -> response
      end
      assert unquote(expected_response) = response
    end
  end

  setup do
    parameters = [hostname: "localhost", database: "cloaktest"]
    {:ok, conn} = Mongo.start_link(parameters)
    Mongo.delete_many(conn, "left", %{})
    Mongo.delete_many(conn, "right", %{})
    for i <- 1..20 do
      Mongo.insert_one!(conn, "left", %{id: i, name: "user#{i}", age: 30})
      Mongo.insert_one!(conn, "right", %{id: i, salary: "#{rem(i, 3)*100}"})
    end
    for i <- 21..25 do
      Mongo.insert_one!(conn, "left", %{id: i})
    end
    decoder = %{method: "text_to_integer", columns: ["salary"]}
    tables_config = [
      %{db_name: "left", name: "left", columns: [], user_id: "id", projection: nil, decoders: []},
      %{db_name: "right", name: "right", columns: [], user_id: "id", projection: nil, decoders: [decoder]}
    ]
    tables =
      tables_config
      |> Enum.flat_map(&MongoDB.load_tables(conn, &1))
      |> Enum.map(&Cloak.Query.DataDecoder.init/1)
      |> Enum.map(&{&1.name, &1})
      |> Enum.into(%{})
    GenServer.stop(conn)

    data_source = %{
      global_id: :"data_source_#{:erlang.unique_integer()}",
      driver: MongoDB,
      parameters: parameters,
      tables: tables
    }
    {:ok, data_source: data_source}
  end

  test "inner join with tables in top-query", context do
    assert_query context, """
        SELECT AVG(salary) FROM "left" INNER JOIN "right" ON "left".id = "right".id WHERE age = 30
      """, %{rows: [%{occurrences: 1, row: [95.0]}]}
  end

  test "left join with table and sub-query in top-query", context do
    assert_query context, """
        SELECT age FROM "left" INNER JOIN (SELECT id AS rid, salary FROM "right") AS t ON id = rid
      """, %{rows: [%{occurrences: 20, row: [30]}]}
  end

  test "left join with table and filtered sub-query in filtered top-query", context do
    assert_query context, """
        SELECT age FROM "left" INNER JOIN
        (SELECT id AS rid, salary FROM "right" WHERE salary >= 0 AND salary < 500) AS t
        ON id = rid WHERE salary = 100
      """, %{rows: [%{occurrences: 7, row: [30]}]}
  end

  test "full join with tables in sub-query", context do
    assert_query context, """
        SELECT COUNT(name) FROM
        (SELECT "left".id, name, salary FROM "left" FULL JOIN "right" ON "left".id = "right".id) AS t
        WHERE salary IN (100, 200)
      """, %{rows: [%{occurrences: 1, row: [14]}]}
  end

  test "function in inner join condition", context do
    assert_query context, """
        SELECT AVG(salary) FROM "left" INNER JOIN "right" ON "left".id = "right".id AND age + 1 = 31
      """, %{rows: [%{occurrences: 1, row: [95.0]}]}
  end
end
