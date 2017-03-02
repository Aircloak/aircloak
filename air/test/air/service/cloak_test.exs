defmodule Air.Service.Cloak.Test do
  use ExUnit.Case, async: false
  use Air.SchemaCase

  import Air.AssertionHelper

  alias Air.{Repo, Schemas.DataSource, Service.Cloak}

  @data_source_id "data_source_id"
  @data_sources [%{"global_id" => @data_source_id, "tables" => []}]

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "should register data sources in the database" do
    Cloak.register_cloak(cloak_info(), @data_sources)
    assert Repo.get_by!(DataSource, global_id: @data_source_id).global_id == @data_source_id
  end

  test "re-registering doesn't add multiple copies of the same data source" do
    Enum.map(1..10, fn(_) -> start_cloak_channel(cloak_info(), @data_sources) end)
    assert length(Repo.all(DataSource, global_id: @data_source_id)) == 1
  end

  test "should return an empty list of channel_pids for a data source with no cloaks" do
    assert [] == Cloak.channel_pids("missing data source")
  end

  test "should return a cloak channel pid given a registered data source" do
    Cloak.register_cloak(cloak_info(), @data_sources)
    assert [self()] == Cloak.channel_pids(@data_source_id) |> Enum.map(& elem(&1, 0))
  end

  test "should allow assigning multiple cloaks to the same data source" do
    assert [{:ok, _pid1}, {:ok, _pid2}] =
      Enum.map(1..2, fn(_) -> start_cloak_channel(cloak_info(), @data_sources) end)
  end

  test "should unregister cloak when channel closes" do
    {:ok, pid} = start_cloak_channel(cloak_info(), @data_sources)
    Process.unlink(pid)
    Process.exit(pid, :exit)

    assert soon([] == Cloak.channel_pids(@data_source_id))
  end

  test "should unregister cloak when channel closes, but retain alternative cloaks" do
    {:ok, pid1} = start_cloak_channel(cloak_info(), @data_sources)
    cloak_info = cloak_info()
    {:ok, pid2} = start_cloak_channel(cloak_info, @data_sources)

    Process.unlink(pid1)
    Process.exit(pid1, :exit)

    assert soon([pid2] == Cloak.channel_pids(@data_source_id) |> Enum.map(& elem(&1, 0)))
  end

  test "should be able to tell when a data source is available" do
    Cloak.register_cloak(cloak_info(), @data_sources)
    assert Cloak.available?(@data_source_id)
  end

  test "should be able to tell when a data source is not available" do
    refute Cloak.available?(@data_source_id)
  end

  test "returns a list of cloaks and their data sources" do
    cloak_info = cloak_info()
    Cloak.register_cloak(cloak_info, @data_sources)
    [cloak] = Cloak.cloaks()
    assert cloak.id == cloak_info.id
    assert cloak.name == cloak_info.name
    assert cloak.data_source_ids == [@data_source_id]
  end

  defp cloak_info() do
    %{
      id: "cloak_id_#{:erlang.unique_integer()}",
      name: "cloak_name",
      online_since: Timex.now()
    }
  end

  defp start_cloak_channel(cloak_info, data_sources) do
    parent = self()
    ref = make_ref()

    pid = spawn_link(fn ->
      registration_result = Cloak.register_cloak(cloak_info, data_sources)
      send(parent, {ref, registration_result})
      :timer.sleep(:infinity)
    end)

    receive do
      {^ref, registration_result} -> {registration_result, pid}
    end
  end
end
