defmodule Cloak.DataSource.ConnectionPoolTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource.Connection.Pool

  setup_all do
    :ok = Cloak.Test.DB.create_table("test_pool", "intval INTEGER")
  end

  setup do
    # Restarting the pool after every test to make sure there are no leftover connections from previous test.
    restart_pool()
  end

  test "checking out reuses an idle conn" do
    conn = Pool.checkout(data_source())
    Pool.checkin(Pool.pool_server(data_source()), conn)
    assert Pool.checkout(data_source()) == conn
  end

  test "new conn is used if no conns are available" do
    conn = Pool.checkout(data_source())
    refute Pool.checkout(data_source()) == conn
  end

  test "conn is dropped after it's been idle for awhile" do
    with_short_connection_keep_time(fn ->
      conn = Pool.checkout(data_source())
      Pool.checkin(Pool.pool_server(data_source()), conn)

      mref = Process.monitor(conn)
      assert_receive {:DOWN, ^mref, _, _, _}
      refute Pool.checkout(data_source()) == conn
    end)
  end

  test "conn is dropped after it's been returned to the pool, and been idle for awhile" do
    with_short_connection_keep_time(fn ->
      conn = Pool.checkout(data_source())
      Pool.checkin(Pool.pool_server(data_source()), conn)

      {:ok, chunks_stream} =
        Cloak.Sql.Parser.parse!("select * from test_pool")
        |> Cloak.Sql.Compiler.compile!(data_source(), [], %{})
        |> Cloak.Sql.Query.resolve_db_columns()
        |> Cloak.DataSource.Connection.chunks()

      Stream.run(chunks_stream)

      mref = Process.monitor(conn)
      assert_receive {:DOWN, ^mref, _, _, _}
      refute Pool.checkout(data_source()) == conn
    end)
  end

  test "conn is returned to the pool if the client process terminates" do
    test_pid = self()

    {_pid, mref} =
      spawn_monitor(fn ->
        conn = Pool.checkout(data_source())
        Pool.checkin(Pool.pool_server(data_source()), conn)
        send(test_pid, {:conn, conn})

        Cloak.Sql.Parser.parse!("select * from test_pool")
        |> Cloak.Sql.Compiler.compile!(data_source(), [], %{})
        |> Cloak.Sql.Query.resolve_db_columns()
        |> Cloak.DataSource.Connection.chunks()

        client = self()
        spawn(fn -> Process.exit(client, :exit) end)
        Process.sleep(:infinity)
      end)

    assert_receive {:DOWN, ^mref, _, _, _}
    assert_receive {:conn, conn}

    # sleep awhile to let the checkin finish first
    Process.sleep(100)
    assert Pool.checkout(data_source()) == conn
  end

  test "connection failure" do
    with_short_connection_timeout(fn ->
      ExUnit.CaptureLog.capture_log(fn ->
        assert_raise(Cloak.Query.ExecutionError, ~r/Failed to establish a connection to the database/, fn ->
          Cloak.Sql.Parser.parse!("select * from test_pool")
          |> Cloak.Sql.Compiler.compile!(data_source(%{hostname: "invalid_host"}), [], %{})
          |> Cloak.Sql.Query.resolve_db_columns()
          |> Cloak.DataSource.Connection.chunks()
        end)
      end)
    end)
  end

  test "SQL error is properly reported" do
    assert_raise(
      Cloak.Query.ExecutionError,
      ~r/column "foobar" does not exist/,
      fn ->
        ExUnit.CaptureLog.capture_log(fn ->
          Cloak.Sql.Parser.parse!("select * from test_pool")
          |> Cloak.Sql.Compiler.compile!(data_source(), [], %{})
          |> Cloak.Sql.Query.resolve_db_columns()
          |> Map.put(:test_fake_statement, "select foobar")
          |> Cloak.DataSource.Connection.chunks()
        end)
      end
    )
  end

  defp restart_pool() do
    pool_parent =
      Pool
      |> Process.whereis()
      |> Process.info(:dictionary)
      |> elem(1)
      |> Keyword.get(:"$ancestors")
      |> hd()

    Supervisor.terminate_child(pool_parent, Pool)
    Supervisor.restart_child(pool_parent, Pool)

    :ok
  end

  defp data_source(extra_params \\ %{}) do
    Cloak.DataSource.all()
    |> hd()
    |> update_in([:parameters], &Map.merge(&1, extra_params))
  end

  defp with_short_connection_timeout(fun) do
    connect_retries = Application.get_env(:cloak, :connect_retries)
    data_source_config = Application.get_env(:cloak, :data_source)

    Application.put_env(:cloak, :connect_retries, 0)
    Application.put_env(:cloak, :data_source, Keyword.put(data_source_config, :connect_timeout, 50))

    try do
      fun.()
    after
      Application.put_env(:cloak, :connect_retries, connect_retries)
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
