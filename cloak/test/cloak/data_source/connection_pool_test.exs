defmodule Cloak.DataSource.ConnectionPoolTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource.Connection.Pool

  import Cloak.Test.QueryHelpers, only: [default_data_source: 0]

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

  test "checking out with `force_new_connection: true` starts a new connection" do
    conn = Pool.checkout(data_source())
    Pool.checkin(Pool.pool_server(data_source()), conn)

    refute Pool.checkout(data_source(), force_new_connection: true) == conn
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

  test "connections are cleaned up on demand" do
    conn = Pool.checkout(data_source())
    Pool.checkin(Pool.pool_server(data_source()), conn)
    Pool.cleanup(data_source())
    refute Pool.checkout(data_source()) == conn
  end

  test "conn is dropped after it's been returned to the pool, and been idle for awhile" do
    with_short_connection_keep_time(fn ->
      conn = Pool.checkout(data_source())
      Pool.checkin(Pool.pool_server(data_source()), conn)

      {:ok, rows_stream} =
        Cloak.Sql.Parser.parse!("select intval from test_pool")
        |> Cloak.Sql.Compiler.compile!(nil, data_source(), [], %{})
        |> Cloak.Sql.Query.resolve_db_columns()
        |> Cloak.DataSource.Streamer.rows()

      Stream.run(rows_stream)

      mref = Process.monitor(conn)
      assert_receive {:DOWN, ^mref, _, _, _}
      refute Pool.checkout(data_source()) == conn
    end)
  end

  test "conn is not returned to the pool if the client process terminates" do
    test_pid = self()

    pid =
      spawn(fn ->
        conn = Pool.checkout(data_source())
        Pool.checkin(Pool.pool_server(data_source()), conn)

        Cloak.Sql.Parser.parse!("select intval from test_pool")
        |> Cloak.Sql.Compiler.compile!(nil, data_source(), [], %{})
        |> Cloak.Sql.Query.resolve_db_columns()
        |> Cloak.DataSource.Streamer.rows()

        send(test_pid, {:conn, conn})
        Process.sleep(:infinity)
      end)

    assert_receive {:conn, conn}
    mref = Process.monitor(conn)
    Process.exit(pid, :kill)
    assert_receive {:DOWN, ^mref, _, _, _}
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
    default_data_source()
    |> update_in([:parameters], &Map.merge(&1, extra_params))
  end

  defp with_short_connection_keep_time(fun) do
    old_data_source_config = Application.get_env(:cloak, :data_source)
    new_data_source_config = Keyword.replace!(old_data_source_config, :connection_keep_time, 50)
    Application.put_env(:cloak, :data_source, new_data_source_config)

    try do
      fun.()
    after
      Application.put_env(:cloak, :data_source, old_data_source_config)
    end
  end
end
