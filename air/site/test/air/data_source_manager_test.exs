defmodule Air.DataSourceManager.Test do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use ExUnit.Case, async: false
  use Air.ModelCase

  import Air.{AssertionHelper, TestUtils}

  alias Air.{Repo, DataSource, DataSourceManager}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  @data_source_id "data_source_id"
  @data_sources [%{"id" => @data_source_id, "name" => "data_source_name", "tables" => []}]

  test "should register data sources in the database" do
    DataSourceManager.register_cloak(cloak_info(), @data_sources)
    assert Repo.get_by!(DataSource, unique_id: @data_source_id).unique_id == @data_source_id
  end

  test "re-registering doesn't add multiple copies of the same data source" do
    DataSourceManager.register_cloak(cloak_info(), @data_sources)
    DataSourceManager.register_cloak(cloak_info(), @data_sources)
    assert length(Repo.all(DataSource, unique_id: @data_source_id)) == 1
  end

  test "should return an empty list of channel_pids for a data source with no cloaks" do
    assert [] == DataSourceManager.channel_pids("missing data source")
  end

  test "should return a cloak channel pid given a registered data source" do
    {terminator, pid} = temporary_process()
    DataSourceManager.register_cloak(cloak_info(pid), @data_sources)
    assert [pid] == DataSourceManager.channel_pids(@data_source_id)
    terminator.()
  end

  test "should allow assigning multiple cloaks to the same data source" do
    {terminator1, pid1} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(cloak_info(pid1), @data_sources)
    {terminator2, pid2} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(cloak_info(pid2), @data_sources)
    terminator1.()
    terminator2.()
  end

  test "should unregister cloak when channel closes" do
    {terminator, pid} = temporary_process()
    DataSourceManager.register_cloak(cloak_info(pid), @data_sources)
    terminator.()
    assert soon([] == DataSourceManager.channel_pids(@data_source_id))
  end

  test "should unregister cloak when channel closes, but retain alternative cloaks" do
    {terminator1, pid1} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(cloak_info(pid1), @data_sources)
    {terminator2, pid2} = temporary_process()
    assert :ok == DataSourceManager.register_cloak(cloak_info(pid2), @data_sources)
    terminator1.()
    assert soon([pid2] == DataSourceManager.channel_pids(@data_source_id))
    terminator2.()
  end

  test "should be able to tell when a data source is available" do
    DataSourceManager.register_cloak(cloak_info(), @data_sources)
    assert DataSourceManager.available?(@data_source_id)
  end

  test "should be able to tell when a data source is not available" do
    refute DataSourceManager.available?(@data_source_id)
  end

  test "returns a list of cloaks and their data sources" do
    DataSourceManager.register_cloak(cloak_info(), @data_sources)
    [cloak] = DataSourceManager.cloaks()
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
