defmodule Cloak.DataSource.DsProxyTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.DsProxy
  alias Cloak.Query.Result

  setup do
    data_source_id = :"data_source_#{:erlang.unique_integer()}"
    bypass = Bypass.open
    url = "http://localhost:#{bypass.port}"
    {:ok, data_source_id: data_source_id, url: url, bypass: bypass}
  end

  test "get columns", context do
    expect_json_post(context.bypass, "/show_columns",
      fn(payload) ->
        assert %{"table" => "table_name"} == payload

        {200, %{
          success: true,
          columns: [%{name: "column1", type: "varchar"}, %{name: "column2", type: "integer"}]
        }}
      end
    )

    columns = DsProxy.describe_table([url: context.url], "table_name")
    assert [{"column1", :text}, {"column2", :integer}] == columns
  end

  test "parsed select count(foo)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["\"bar\".\"user_id\"", "\"bar\".\"foo\""], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT \"bar\".\"user_id\",\"bar\".\"foo\" FROM \"bar\""} == statement

        {200, %{success: true, columns: ["bar.user_id", "bar.foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from bar")
    assert {:ok, %Result{columns: ["count"], buckets: [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "parsed select count(*)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["\"bar\".\"user_id\""], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" => query_string} = statement
        assert "SELECT \"bar\".\"user_id\" FROM \"bar\"" == query_string

        {200, %{success: true, columns: ["bar.user_id"], rows: Enum.map(1..100, &[&1])}}
      end
    )

    query_result = run_query(context, "select count(*) from bar")
    assert {:ok, %Result{columns: ["count"], buckets: [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "parsed column deduplication", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["\"bar\".\"user_id\"", "\"bar\".\"foo\"", "\"bar\".\"baz\""],
         "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT \"bar\".\"user_id\",\"bar\".\"foo\",\"bar\".\"baz\" FROM \"bar\""} == statement

        {200, %{success: true, columns: ["bar.user_id", "bar.foo", "bar.baz"],
          rows: Enum.map(1..100, &[&1, 1, 2])}}
      end
    )

    query_result = run_query(context, "select foo, baz, foo, baz from \"bar\"")
    assert {:ok, %Result{columns: columns, buckets: [row]}, []} = query_result
    assert ["foo", "baz", "foo", "baz"] == columns
    assert %{occurrences: 100, row: [1, 2, 1, 2]} == row
  end

  test "deduplication of aggregate columns", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["\"bar\".\"user_id\"", "\"bar\".\"foo\""], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT \"bar\".\"user_id\",\"bar\".\"foo\" FROM \"bar\""} == statement

        rows = Enum.map(1..49, &[&1, 0]) ++ Enum.map(50..100, &[&1, 10])
        {200, %{success: true, columns: ["bar.user_id", "bar.foo"], rows: rows}}
      end
    )

    query_result = run_query(context, "select min(foo), max(foo) from bar")
    assert {:ok, %Result{buckets: [bucket], columns: columns}, []} = query_result
    assert ["min", "max"] == columns
    assert %{occurrences: 1, row: [0.0, 10.0]} == bucket
  end

  test "dsproxy returns empty set", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["\"bar\".\"user_id\"", "\"bar\".\"foo\""], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT \"bar\".\"user_id\",\"bar\".\"foo\" FROM \"bar\""} == statement

        {200, %{success: true, columns: ["bar.user_id", "bar.foo"], rows: [[]]}}
      end
    )

    query_result = run_query(context, "select foo from bar")
    assert {:ok, %Result{columns: ["foo"], buckets: []}, []} = query_result
  end

  test "unsafe select count(foo)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"statement" => statement} = payload
        assert %{
          "type" => "unsafe",
          "val" => "SELECT \"__aircloak_user_id__\",\"foo\" FROM (select foo from bar) AS unsafe_subquery"
        } = statement

        {200, %{success: true, columns: ["user_id", "foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from (select foo from bar) as baz")
    assert {:ok, %Result{columns: ["count"], buckets: [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "unsafe select count(*)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"statement" => statement} = payload
        assert %{
          "type" => "unsafe",
          "val" => "SELECT \"__aircloak_user_id__\" FROM (select * from bar) AS unsafe_subquery"
        } = statement

        {200, %{success: true, columns: ["user_id"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select count(*) from (select * from bar) as baz")
    assert {:ok, %Result{columns: ["count"], buckets: [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "unsafe select count(*) group by foo", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"statement" => statement} = payload
        assert %{
          "type" => "unsafe",
          "val" => "SELECT \"__aircloak_user_id__\",\"foo\" FROM (select foo from bar) AS unsafe_subquery"
        } = statement

        {200, %{success: true, columns: ["user_id", "foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select foo, count(*) from (select foo from bar) as \"baz\" group by foo")
    assert {:ok, %Result{columns: ["foo", "count"], buckets: [%{occurrences: 1, row: [:*, 100]}]}, []} = query_result
  end

  test "function types are not verified in an unparsed query", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"statement" => statement} = payload
        assert %{
          "type" => "unsafe",
          "val" => "SELECT \"__aircloak_user_id__\",\"c\" FROM (select count(*) as c from foo) AS unsafe_subquery"
        } = statement
        {200, %{success: true, columns: ["user_id", "c"], rows: Enum.map(1..100, &[&1, 100])}}
      end
    )

    query_result = run_query(context, "select max(c) from (select count(*) as c from foo) as sq")
    assert {:ok, %Result{columns: ["max"], buckets: [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "propagating reported error", context do
    expect_json_post(context.bypass, "/query",
      fn(_) ->
        {200, %{success: false, error: "Some error message"}}
      end
    )

    assert {:error, "Some error message"} == run_query(context, "select foo from (bar) as baz")
  end

  test "can't join subqueries in dsproxy", context do
    assert {:error, error} = run_query(context, "select foo from bar, (select foo from baz) sq")
    assert error == "Joining subqueries is not supported for this data source."
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp expect_json_post(bypass, path, callback) do
    Bypass.expect(bypass,
      fn(conn) ->
        assert "POST" == conn.method
        assert path == conn.request_path

        headers = Map.new(conn.req_headers)
        assert "application/json" == headers["content-type"]

        {:ok, body, conn} = Plug.Conn.read_body(conn)

        {status, response} = callback.(Poison.decode!(body))
        Plug.Conn.resp(conn, status, Poison.encode!(response))
      end
    )
  end

  defp run_query(test_context, statement, data_source_opts \\ []) do
    default_data_source(test_context)
    |> Map.merge(Enum.into(data_source_opts, %{}))
    |> Cloak.Query.Runner.run_sync(statement)
  end

  defp default_data_source(test_context) do
    %{
      global_id: test_context.data_source_id,
      driver: DsProxy,
      salt: "test-salt",
      parameters: [url: test_context.url],
      tables: %{
        bar: %{
          name: "bar",
          db_name: "bar",
          user_id: "user_id",
          columns: %{"user_id" => :integer, "foo" => :integer, "baz" => :integer}
        }
      }
    }
  end
end
