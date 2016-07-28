defmodule Cloak.DataSource.DsProxyTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.DsProxy

  setup do
    data_source_id = :"data_source_#{:erlang.unique_integer()}"
    bypass = Bypass.open
    url = "http://localhost:#{bypass.port}"
    {:ok, _} = DsProxy.start_link(data_source_id, url: url)
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

    columns = DsProxy.get_columns(context.data_source_id, "table_name")
    assert [{"column1", :text}, {"column2", :integer}] == columns
  end

  test "parsed select count(foo)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["bar.user_id", "bar.foo"], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT bar.user_id AS \"bar.user_id\",bar.foo AS \"bar.foo\" FROM bar "} == statement

        {200, %{success: true, columns: ["bar.user_id", "bar.foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from bar")
    assert {:ok, {:buckets, ["count"], [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "parsed select count(*)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["bar.user_id"], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" => query_string} = statement
        assert "SELECT bar.user_id AS \"bar.user_id\" FROM bar " == query_string

        {200, %{success: true, columns: ["bar.user_id"], rows: Enum.map(1..100, &[&1])}}
      end
    )

    query_result = run_query(context, "select count(*) from bar")
    assert {:ok, {:buckets, ["count"], [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "parsed column deduplication", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["bar.user_id", "bar.foo", "bar.baz"], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT bar.user_id AS \"bar.user_id\",bar.foo AS \"bar.foo\",bar.baz AS \"bar.baz\" FROM bar "} == statement

        {200, %{success: true, columns: ["bar.user_id", "bar.foo", "bar.baz"],
          rows: Enum.map(1..100, &[&1, 1, 2])}}
      end
    )

    query_result = run_query(context, "select foo, baz, foo, baz from bar")
    assert {:ok, {:buckets, columns, [row]}, []} = query_result
    assert ["foo", "baz", "foo", "baz"] == columns
    assert %{occurrences: 100, row: [1, 2, 1, 2]} == row
  end

  test "deduplication of aggregate columns", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["bar.user_id", "bar.foo"], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT bar.user_id AS \"bar.user_id\",bar.foo AS \"bar.foo\" FROM bar "} == statement

        rows = Enum.map(1..49, &[&1, 0]) ++ Enum.map(50..100, &[&1, 10])
        {200, %{success: true, columns: ["user_id", "bar.foo"], rows: rows}}
      end
    )

    query_result = run_query(context, "select min(foo), max(foo) from bar")
    assert {:ok, {:buckets, columns, [row]}, []} = query_result
    assert ["min", "max"] == columns
    assert %{occurrences: 1, row: [0.0, 10.0]} == row
  end

  test "dsproxy returns empty set", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"columns" => ["bar.user_id", "bar.foo"], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" =>
          "SELECT bar.user_id AS \"bar.user_id\",bar.foo AS \"bar.foo\" FROM bar "} == statement

        {200, %{success: true, columns: ["user_id", "bar.foo"], rows: [[]]}}
      end
    )

    query_result = run_query(context, "select foo from bar")
    assert {:ok, {:buckets, ["foo"], []}, []} = query_result
  end

  test "unsafe select count(foo)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"statement" => statement} = payload
        assert %{"type" => "unsafe", "val" =>
          "select foo from bar"} == statement

        {200, %{success: true, columns: ["user_id", "foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from (select foo from bar) as baz")
    assert {:ok, {:buckets, ["count"], [%{occurrences: 1, row: [100]}]}, []} = query_result
  end

  test "unsafe select count(*)", context do
    expect_json_post(context.bypass, "/query",
      fn(payload) ->
        assert %{"statement" => statement} = payload
        assert %{"type" => "unsafe", "val" => "select foo from bar"} == statement

        {200, %{success: true, columns: ["user_id", "foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select foo, count(*) from (select foo from bar) as baz group by foo")
    assert {:ok, {:buckets, ["foo", "count"], [%{occurrences: 1, row: [:*, 100]}]}, []} = query_result
  end

  test "invalid select column in unsafe select", context do
    expect_json_post(context.bypass, "/query",
      fn(_) ->
        {200, %{success: true, columns: ["user_id", "foo1", "foo2"], rows: Enum.map(1..100, &[&1, &1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from (select foo1 from bar) as baz")
    assert {:error, message} = query_result
    assert "Column `foo` doesn't exist in selected columns." == message
  end

  test "invalid group by in unsafe select", context do
    expect_json_post(context.bypass, "/query",
      fn(_) ->
        {200, %{success: true, columns: ["user_id", "foo1", "foo2"], rows: Enum.map(1..100, &[&1, &1, &1])}}
      end
    )

    query_result = run_query(context, "select count(*) from (select foo1 from bar) as baz group by foobar")
    assert {:error, message} = query_result
    assert "Column `foobar` doesn't exist in selected columns." == message
  end

  test "propagating reported error", context do
    expect_json_post(context.bypass, "/query",
      fn(_) ->
        {200, %{success: false, error: "Some error message"}}
      end
    )

    assert {:error, "Some error message"} == run_query(context, "select foo from (bar) as baz")
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
    %Cloak.Query{
      id: "foobar",
      statement: statement,
      data_source:
        default_data_source(test_context)
        |> Map.merge(Enum.into(data_source_opts, %{}))
    }
    |> Cloak.Query.Runner.run()
  end

  defp default_data_source(test_context) do
    %{
      id: test_context.data_source_id,
      driver: DsProxy,
      salt: "test-salt",
      parameters: [url: test_context.url],
      tables: %{
        bar: %{
          name: "bar",
          user_id: "user_id",
          columns: %{"foo" => :integer, "baz" => :integer}
        }
      }
    }
  end
end
