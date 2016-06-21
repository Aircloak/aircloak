defmodule Cloak.DataSource.AclTest do
  use ExUnit.Case, async: true

  alias Cloak.DataSource.Acl

  setup do
    data_source_id = :"data_source_#{:erlang.unique_integer()}"
    bypass = Bypass.open
    url = "http://localhost:#{bypass.port}"
    {:ok, _} = Acl.start_link(data_source_id, %{url: url})
    {:ok, data_source_id: data_source_id, url: url, bypass: bypass}
  end

  test "get columns", context do
    expect_json_post(context, "/list_columns",
      fn(payload) ->
        assert %{"table" => "table_name"} == payload

        {200, %{
          success: true,
          columns: [%{name: "column1", type: "varchar"}, %{name: "column2", type: "integer"}]
        }}
      end
    )

    columns = Acl.get_columns(context.data_source_id, "table_name")
    assert [{"column1", :text}, {"column2", :integer}] == columns
  end

  test "parsed select", context do
    expect_json_post(context, "/run_query",
      fn(payload) ->
        assert %{"columns" => ["user_id", "foo"], "statement" => statement} = payload
        assert %{"params" => [], "type" => "parsed", "val" => "SELECT user_id,foo FROM bar "} == statement

        {200, %{success: true, columns: ["user_id", "foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from bar")
    assert {:ok, {:buckets, ["count(foo)"], [%{occurrences: 1, row: [100]}]}} = query_result
  end

  test "unsafe select", context do
    expect_json_post(context, "/run_query",
      fn(payload) ->
        assert %{"columns" => ["foo"], "statement" => statement} = payload
        assert %{"type" => "unsafe", "val" => "select foo from bar"} == statement

        {200, %{success: true, columns: ["user_id", "foo"], rows: Enum.map(1..100, &[&1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from (select foo from bar) as baz")
    assert {:ok, {:buckets, ["count(foo)"], [%{occurrences: 1, row: [100]}]}} = query_result
  end

  test "invalid unsafe select", context do
    expect_json_post(context, "/run_query",
      fn(payload) ->
        {200, %{success: true, columns: ["user_id", "foo1", "foo2"], rows: Enum.map(1..100, &[&1, &1, &1])}}
      end
    )

    query_result = run_query(context, "select count(foo) from (select foo1 from bar) as baz")
    assert {:error, message} = query_result
    assert "Column `foo` doesn't exist in selected columns `foo1`, `foo2`." == message

    query_result = run_query(context, "select count(*) from (select foo1 from bar) as baz group by foobar")
    assert {:error, message} = query_result
    assert "Column `foobar` doesn't exist in selected columns `foo1`, `foo2`." == message
  end


  ## ----------------------------------------------------------------
  ## Internal functions
  ## ----------------------------------------------------------------

  defp expect_json_post(test_context, path, callback) do
    Bypass.expect(test_context.bypass,
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
      driver: Acl,
      parameters: %{url: test_context.url},
      tables: %{
        bar: %{
          name: "bar",
          user_id: "user_id",
          columns: %{"foo" => :integer}
        }
      }
    }
  end
end
