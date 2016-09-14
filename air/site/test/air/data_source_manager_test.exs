defmodule Air.DataSourceManager.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use ExUnit.Case, async: false
  use Air.ModelCase

  import Air.{AssertionHelper, TestUtils}

  alias Air.{Repo, DataSource, DataSourceManager}

  setup do
    {:ok, server} = DataSourceManager.start_link(__MODULE__)
    Ecto.Adapters.SQL.Sandbox.allow(Repo, self(), server)

    {:ok, server: server}
  end

  @data_source_id "data_source_id"
  @data_sources [%{"global_id" => @data_source_id, "tables" => []}]

  test "should register data sources in the database", %{server: server} do
    DataSourceManager.register_cloak(server, cloak_info(), @data_sources)
    assert Repo.get_by!(DataSource, global_id: @data_source_id).global_id == @data_source_id
  end

  test "re-registering doesn't add multiple copies of the same data source", %{server: server} do
    DataSourceManager.register_cloak(server, cloak_info(), @data_sources)
    DataSourceManager.register_cloak(server, cloak_info(), @data_sources)
    assert length(Repo.all(DataSource, global_id: @data_source_id)) == 1
  end

  test "should return an empty list of channel_pids for a data source with no cloaks", %{server: server} do
    assert [] == DataSourceManager.channel_pids(server, "missing data source")
  end

  test "should return a cloak channel pid given a registered data source", %{server: server} do
    {terminator, pid} = temporary_process()
    DataSourceManager.register_cloak(server, cloak_info(pid), @data_sources)
    assert [pid] == DataSourceManager.channel_pids(server, @data_source_id)
    terminator.()
  end

  test "should allow assigning multiple cloaks to the same data source", %{server: server} do
    {terminator1, pid1} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(server, cloak_info(pid1), @data_sources)
    {terminator2, pid2} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(server, cloak_info(pid2), @data_sources)
    terminator1.()
    terminator2.()
  end

  test "should unregister cloak when channel closes", %{server: server} do
    {terminator, pid} = temporary_process()
    DataSourceManager.register_cloak(server, cloak_info(pid), @data_sources)
    terminator.()
    assert soon([] == DataSourceManager.channel_pids(server, @data_source_id))
  end

  test "should unregister cloak when channel closes, but retain alternative cloaks", %{server: server} do
    {terminator1, pid1} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(server, cloak_info(pid1), @data_sources)
    {terminator2, pid2} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(server, cloak_info(pid2), @data_sources)
    terminator1.()
    assert soon([pid2] == DataSourceManager.channel_pids(server, @data_source_id))
    terminator2.()
  end

  test "should be able to tell when a data source is available", %{server: server} do
    DataSourceManager.register_cloak(server, cloak_info(), @data_sources)
    assert DataSourceManager.available?(server, @data_source_id)
  end

  test "should be able to tell when a data source is not available", %{server: server} do
    refute DataSourceManager.available?(server, @data_source_id)
  end

  test "returns a list of cloaks and their data sources", %{server: server} do
    DataSourceManager.register_cloak(server, cloak_info(), @data_sources)
    [cloak] = DataSourceManager.cloaks(server)
    assert cloak.id == cloak_info().id
    assert cloak.name == cloak_info().name
    assert cloak.channel_pid == self()
    assert cloak.data_source_ids == [@data_source_id]
  end

  test "removes entries for which there are no cloaks" do
    pid = :pid
    cloak_info = %{channel_pid: pid}
    state = %{
      data_source_to_cloak: %{data_source: [cloak_info]}
    }
    assert %{data_source_to_cloak: %{}} === DataSourceManager.do_remove_disconnected_cloak(pid, state)
  end

  defp cloak_info(pid \\ self()) do
    %{
      channel_pid: pid,
      id: "cloak_id",
      name: "cloak_name",
      online_since: Timex.DateTime.now()
    }
  end
end
