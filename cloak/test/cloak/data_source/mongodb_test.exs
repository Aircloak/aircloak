if :os.cmd('ps -edaf | grep mongo | grep -v grep') != '' do
defmodule Cloak.DataSource.MongoDBTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.MongoDB
  alias Cloak.Query.Runner

  @table "test"

  defmacro assert_query(context, query, parameters \\ [], expected_response) do
    quote do
      Runner.start("1", unquote(context).data_source, unquote(query), unquote(parameters), {:process, self()})
      response = receive do
        {:reply, response} -> response
      end
      assert unquote(expected_response) = response
    end
  end

  setup do
    parameters = [hostname: "localhost", database: "cloaktest"]
    {:ok, conn} = Mongo.start_link(parameters)
    Mongo.delete_many(conn, @table, %{})
    for i <- 1..10 do
      value = %{name: "user#{i}", age: 30, male: true, bills: [%{issuer: "vendor", ids: [1, 2]}]}
      Mongo.insert_one!(conn, @table, value)
    end
    for _i <- 11..15 do
      Mongo.insert_one!(conn, @table, %{})
    end
    tables =
      conn
      |> MongoDB.load_tables(%{db_name: @table, name: @table, columns: [], user_id: "_id"})
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

  test "schema mapping", context do
    %{@table => root, @table <> "_bills" => bills, @table <> "_bills_ids" => ids} = context.data_source.tables
    assert root.columns == [
      {"_id", :text},
      {"age", :real},
      {"bills#", :integer},
      {"male", :boolean},
      {"name", :text}
    ]
    assert bills.columns == [
      {"_id", :text},
      {"age", :real},
      {"male", :boolean},
      {"name", :text},
      {"bills.ids#", :integer},
      {"bills.issuer", :text}
    ]
    assert ids.columns == [
      {"_id", :text},
      {"age", :real},
      {"male", :boolean},
      {"name", :text},
      {"bills.issuer", :text},
      {"bills.ids", :real}
    ]
  end

  test "basic queries on root table", context do
    assert_query context, "SELECT COUNT(name) FROM #{@table}", %{rows: [%{occurrences: 1, row: [10]}]}
    assert_query context, "SELECT AVG(age) FROM #{@table}", %{rows: [%{occurrences: 1, row: [30.0]}]}
    assert_query context, "SELECT DISTINCT male FROM #{@table} WHERE male IS NOT NULL",
      %{rows: [%{occurrences: 1, row: [true]}]}
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
      "SELECT COUNT(*) FROM #{@table}_bills WHERE bills.issuer = 'vendor' AND male = TRUE AND bills.ids# = 2",
      %{rows: [%{occurrences: 1, row: [10]}]}

    assert_query context,
      "SELECT bills.issuer, COUNT(*) FROM #{@table}_bills_ids WHERE bills.ids = 1 GROUP BY bills.issuer",
      %{rows: [%{occurrences: 1, row: ["vendor", 10]}]}
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

  test "functions in sub-queries", context do
    assert_query context, "SELECT AVG(age) FROM (SELECT _id, trunc(abs(age) - 30.0) AS age FROM #{@table}) AS t",
      %{rows: [%{occurrences: 1, row: [0.0]}]}
    assert_query context, "SELECT name FROM (SELECT _id, lower(left(name, 4)) AS name FROM #{@table}) AS t",
      %{rows: [%{occurrences: 5, row: [nil]}, %{occurrences: 10, row: ["user"]}]}
  end

  test "aggregation in sub-queries", context do
    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, COUNT(*) AS value FROM #{@table} GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [15, 15]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, COUNT(age) AS value FROM #{@table} GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [15, 10]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM (SELECT _id, AVG(age) AS value FROM #{@table} GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [15, 300.0]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM
        (SELECT _id, COUNT(DISTINCT bills.issuer) AS value FROM #{@table}_bills_ids GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [10, 10]}]}

    assert_query context, """
        SELECT COUNT(*), SUM(value) FROM
        (SELECT _id, SUM(DISTINCT bills.ids) AS value FROM #{@table}_bills_ids GROUP BY _id) AS t
      """, %{rows: [%{occurrences: 1, row: [10, 30]}]}
  end

  test "distinct in sub-queries", context do
    assert_query context, """
        SELECT COUNT(*) FROM (SELECT DISTINCT _id, length(bills.issuer) AS value FROM #{@table}_bills_ids) AS t
      """, %{rows: [%{occurrences: 1, row: [10]}]}
  end
end
end
