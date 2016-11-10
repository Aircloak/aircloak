defmodule Air.PsqlServer.RanchServerTest do
  use ExUnit.Case, async: true

  alias Air.PsqlServer.RanchServer
  alias Air.PsqlTestDriver.Client
  use Air.PsqlTestDriver

  @port 20_000

  setup_all do
    {:ok, _} = Air.PsqlTestDriver.listen(@port)
    :ok
  end

  test "rejecting a login" do
    {:ok, _} = Client.start_link(@port, "some_user", "some_pass")
    handle_server_event {:login, _password}, _conn, do: :error
    assert_receive {:not_connected, error}
    assert to_string(error) =~ ~r/Authentication failed/
  end

  test "logging in" do
    {:ok, _} = Client.start_link(@port, "some_user", "some_pass")
    handle_server_event {:login, password}, conn do
      assert Map.fetch!(conn.login_params, "user") == "some_user"
      assert password == "some_pass"
      {:ok, conn}
    end
    assert_receive :connected
  end

  test "simple query" do
    {:ok, client} = Client.start_link(@port, "some_user", "some_pass")
    handle_server_event {:login, _password}, conn, do: {:ok, conn}

    Client.simple_query(client, "select foo from bar")
    handle_server_event {:run_query, query, params, max_rows}, conn do
      assert query == "select foo from bar"
      assert params == []
      assert max_rows == 0
      RanchServer.set_query_result(conn, %{columns: [%{name: "foo", type: :int8}], rows: [[1], [2]]})
    end

    assert_receive {:selected, columns, rows}
    assert columns == ['foo']
    assert rows == [{'1'}, {'2'}]
  end

  test "simple query error" do
    {:ok, client} = Client.start_link(@port, "some_user", "some_pass")
    handle_server_event {:login, _password}, conn, do: {:ok, conn}

    Client.simple_query(client, "select foo from bar")
    handle_server_event {:run_query, _query, _params, _max_rows}, conn, do:
      RanchServer.set_query_result(conn, %{error: "some error"})

    assert_receive {:error, error}
    assert to_string(error) =~ ~r/some error/
  end

  test "extended query" do
    {:ok, client} = Client.start_link(@port, "some_user", "some_pass")
    handle_server_event {:login, _password}, conn, do: {:ok, conn}

    Client.extended_query(client, "select $1", [{:sql_integer, [1]}])
    handle_server_event {:describe_statement, query, params}, conn do
      assert query == "select $1"
      assert params == [1]
      RanchServer.set_describe_result(conn, [%{name: "col1", type: :int8}])
    end

    handle_server_event {:run_query, query, params, max_rows}, conn do
      assert query == "select $1"
      assert params == [1]
      assert max_rows == 0
      RanchServer.set_query_result(conn, %{columns: [], rows: [[1]]})
    end

    assert_receive {:selected, ['col1'], [{'1'}]}
  end
end
