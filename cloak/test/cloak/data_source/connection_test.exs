defmodule Cloak.DataSource.ConnectionTest do
  use ExUnit.Case, async: false

  alias Cloak.DataSource.Connection
  alias Cloak.DataSource.Connection.Pool

  import Cloak.Test.QueryHelpers, only: [default_data_source: 0]
  import Aircloak.AssertionHelper

  setup do
    # Restarting the pool after every test to make sure there are no leftover connections from previous test.
    restart_pool()
  end

  test "execute" do
    assert Connection.execute!(data_source(), &Postgrex.query!(&1, "select 1", [])).rows == [[1]]
  end

  test "execute returns the connection to the pool" do
    driver_conn1 = Connection.execute!(data_source(), & &1)

    Process.sleep(50)
    driver_conn2 = Connection.execute!(data_source(), & &1)

    assert driver_conn1 == driver_conn2
  end

  test "if execute client crashes, the connection is returned to the pool" do
    test_pid = self()

    {client, mref} =
      spawn_monitor(fn ->
        Connection.execute!(
          data_source(),
          fn driver_conn ->
            send(test_pid, {:driver_conn, driver_conn})
            Process.sleep(:infinity)
          end
        )
      end)

    assert_receive {:driver_conn, driver_conn}

    Process.exit(client, :shutdown)
    assert_receive {:DOWN, ^mref, _, _, _}

    Process.sleep(50)
    assert ^driver_conn = Connection.execute!(data_source(), & &1)
  end

  test "reporting connect error on execution" do
    with_short_connection_timeout(fn ->
      error_operation = fn -> Connection.execute!(data_source(%{hostname: "invalid_host"}), & &1) end

      soon do
        assert_raise Cloak.Query.ExecutionError, ~r/Failed to establish a connection to the database/, error_operation
      end
    end)
  end

  test "reporting connection timeout" do
    with_short_connection_timeout(0, fn ->
      error_operation = fn -> Connection.execute!(data_source(), & &1) end
      assert_raise Cloak.Query.ExecutionError, "Timeout connecting to the database.", error_operation
    end)
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

  defp with_short_connection_timeout(timeout \\ 50, fun) do
    data_source_config = Application.get_env(:cloak, :data_source)
    Application.put_env(:cloak, :data_source, Keyword.put(data_source_config, :connect_timeout, timeout))

    try do
      fun.()
    after
      Application.put_env(:cloak, :data_source, data_source_config)
    end
  end

  defp data_source(extra_params \\ %{}) do
    default_data_source()
    |> update_in([:parameters], &Map.merge(&1, extra_params))
  end
end
