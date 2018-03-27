defmodule Cloak.DataSource.ConnectionPoolTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource.ConnectionPool

  test "execute! checks out a valid connection" do
    assert is_pid(execute!(& &1))
    assert Process.alive?(execute!(& &1))
  end

  test "execute! returns the lambda result" do
    assert execute!(fn _conn -> :some_result end) == :some_result
  end

  test "execute! reuses an idle conn" do
    conn = execute!(& &1)
    assert execute!(& &1) == conn
  end

  test "new conn is used if no conns are available" do
    execute!(fn conn -> assert execute!(& &1) != conn end)
  end

  test "conn is dropped after it's been idle for awhile" do
    with_short_connection_keep_time(fn ->
      conn = execute!(& &1)

      :timer.sleep(2 * Cloak.DataSource.Driver.connection_keep_time())

      assert execute!(& &1) != conn
      refute Process.alive?(conn)
    end)
  end

  test "conn is not reused on client exception" do
    with_short_connection_keep_time(fn ->
      test_pid = self()

      ExUnit.CaptureLog.capture_log(fn ->
        {_pid, mref} =
          spawn_monitor(fn ->
            execute!(fn conn ->
              send(test_pid, {:conn, conn})
              raise "some error"
            end)
          end)

        assert_receive {:DOWN, ^mref, _, _, _}
        assert_receive {:conn, conn}

        :timer.sleep(div(Cloak.DataSource.Driver.connection_keep_time(), 4))

        assert execute!(& &1) != conn
        refute Process.alive?(conn)
      end)
    end)
  end

  test "conn is not returned if the client is killed" do
    with_short_connection_keep_time(fn ->
      test_pid = self()

      {_pid, mref} =
        spawn_monitor(fn ->
          execute!(fn conn ->
            send(test_pid, {:conn, conn})
            client = self()
            spawn(fn -> Process.exit(client, :crash) end)
            :timer.sleep(:infinity)
          end)
        end)

      assert_receive {:DOWN, ^mref, _, _, _}
      assert_receive {:conn, conn}

      :timer.sleep(div(Cloak.DataSource.Driver.connection_keep_time(), 4))

      assert execute!(& &1) != conn
      refute Process.alive?(conn)
    end)
  end

  test "connection failure" do
    with_short_connection_timeout(fn ->
      assert_raise(Cloak.Query.ExecutionError, "Failed connecting to the database", fn ->
        ExUnit.CaptureLog.capture_log(fn -> execute!(%{hostname: "invalid_host"}, & &1) end)
      end)
    end)
  end

  test "unhandled exception" do
    assert_raise(ArgumentError, "some error", fn ->
      execute!(fn _conn -> raise ArgumentError.exception("some error") end)
    end)
  end

  test "unhandled throw" do
    assert_raise(RuntimeError, ~s/Connection error :throw: "some throw"/, fn ->
      execute!(fn _conn -> throw("some throw") end)
    end)
  end

  defp execute!(extra_params \\ %{}, fun) do
    Cloak.DataSource.all()
    |> hd()
    |> update_in([:parameters], &Map.merge(&1, extra_params))
    |> ConnectionPool.execute!(fun)
  end

  defp with_short_connection_timeout(fun) do
    data_source_config = Application.get_env(:cloak, :data_source)

    Application.put_env(
      :cloak,
      :data_source,
      Keyword.put(data_source_config, :connect_timeout, 50)
    )

    try do
      fun.()
    after
      Application.put_env(:cloak, :data_source, data_source_config)
    end
  end

  defp with_short_connection_keep_time(fun) do
    data_source_config = Application.get_env(:cloak, :data_source)

    Application.put_env(
      :cloak,
      :data_source,
      Keyword.put(data_source_config, :connection_keep_time, 50)
    )

    try do
      fun.()
    after
      Application.put_env(:cloak, :data_source, data_source_config)
    end
  end
end
