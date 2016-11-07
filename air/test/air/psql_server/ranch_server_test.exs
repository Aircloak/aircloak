defmodule Air.PsqlServer.RanchServerTest do
  use ExUnit.Case, async: true
  @moduletag :disabled

  alias Air.PsqlServer.RanchServer
  use Air.PsqlTestDriver

  @port 20_000

  setup_all do
    {:ok, _} = Air.PsqlTestDriver.listen(@port)
    :ok
  end

  test "rejecting a login" do
    client = start_client(@port, "some_user", "some_pass")
    handle_server_event {:login, _password}, _conn, do: :error
    assert {:error, error} = Task.await(client)
    assert to_string(error) =~ ~r/Authentication failed/
  end

  test "logging in" do
    client = start_client(@port, "some_user", "some_pass")
    handle_server_event {:login, password}, conn do
      assert Map.fetch!(conn.login_params, "user") == "some_user"
      assert password == "some_pass"
      {:ok, conn}
    end

    assert :ok == Task.await(client)
  end

  test "successful query" do
    client = start_client(@port, "some_user", "some_pass", "select foo from bar")
    handle_server_event {:login, _password}, conn, do: {:ok, conn}

    handle_server_event {:run_query, query}, conn do
      assert query == "select foo from bar"
      RanchServer.set_query_result(conn, %{columns: [%{name: "foo", type: :integer}], rows: [[1], [2]]})
    end

    assert {:selected, columns, rows} =  Task.await(client)
    assert columns == ['foo']
    assert rows == [{'1'}, {'2'}]
  end

  test "query error" do
    client = start_client(@port, "some_user", "some_pass", "select foo from bar")
    handle_server_event {:login, _password}, conn, do: {:ok, conn}
    handle_server_event {:run_query, _query}, conn, do:
      RanchServer.set_query_result(conn, %{error: "some error"})

    assert {:error, error} =  Task.await(client)
    assert to_string(error) =~ ~r/some error/
  end
end
