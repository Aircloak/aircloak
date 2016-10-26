defmodule Air.PsqlServer.RanchServerTest do
  use ExUnit.Case, async: true

  alias Air.PsqlServer.RanchServer
  import Air.PsqlTestDriver, only: :macros

  @port 20_000

  setup_all do
    {:ok, _} = Air.PsqlTestDriver.listen(@port)
    :ok
  end

  test "rejecting a login" do
    output =
      run_client(@port, "some_user", "some_pass") do
        handle_server_event {:login, _password}, _conn, do: :error
      end
      |> Task.await()

    assert output =~ ~r/Authentication failed/
  end

  test "logging in" do
    run_client(@port, "some_user", "some_pass") do
      handle_server_event {:login, password}, conn do
        assert Map.fetch!(conn.login_params, "user") == "some_user"
        assert password == "some_pass"
        {:ok, conn}
      end
    end
  end

  test "successful query" do
    output =
      run_client(@port, "some_user", "some_pass", "select foo from bar") do
        handle_server_event {:login, _password}, conn, do: {:ok, conn}

        handle_server_event {:run_query, query}, conn do
          assert query == "select foo from bar"
          RanchServer.set_query_result(conn, %{columns: [%{name: "foo", type: :integer}], rows: [[1], [2]]})
        end
      end
      |> Task.await()

    assert output =~ ~r/foo/
    assert output =~ ~r/1\n/
    assert output =~ ~r/2\n/
    assert output =~ ~r/2 rows/
  end
end
