if :os.cmd('ps -edaf | grep mongo | grep -v grep') != '' do
defmodule Cloak.DataSource.MongoDBTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.MongoDB
  alias Cloak.Query.Runner

  @table "test"

  defmacro assert_query(context, query, expected_response) do
    quote do
      Runner.start("1", unquote(context).data_source, unquote(query), {:process, self()})
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
    assert_query context, "SELECT DISTINCT male FROM #{@table}", %{rows: [%{occurrences: 1, row: [true]}]}
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
end
end
