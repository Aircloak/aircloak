defmodule Air.Service.Cloak.Test do
  use ExUnit.Case, async: false
  use Air.SchemaCase

  import Air.AssertionHelper

  alias Air.{Repo, TestRepoHelper, Schemas.DataSource, Service.Cloak}

  @data_source_id "data_source_id"
  @data_source_name "data_source_name"
  @data_source %{"name" => @data_source_name, "global_id" => @data_source_id, "tables" => []}
  @data_sources [@data_source]

  setup do
    wait_for_cleanup()
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
    :ok
  end

  test "should register data sources in the database" do
    Cloak.register(TestRepoHelper.cloak_info(), @data_sources)
    assert Repo.get_by!(DataSource, name: @data_source_name).name == @data_source_name
  end

  test "re-registering doesn't add multiple copies of the same data source" do
    Enum.map(1..10, fn(_) -> start_cloak_channel(@data_sources) end)
    assert length(Repo.all(DataSource, name: @data_source_name)) == 1
  end

  test "should return an empty list of channel_pids for a data source with no cloaks" do
    assert [] == Cloak.channel_pids("missing data source")
  end

  test "should return a cloak channel pid given a registered data source" do
    cloak_info = TestRepoHelper.cloak_info()

    Cloak.register(cloak_info, @data_sources)

    cloak_id = cloak_info.id
    this = self()
    assert [{^this, %{id: ^cloak_id}}] = Cloak.channel_pids(@data_source_name)
  end

  test "should allow assigning multiple cloaks to the same data source" do
    assert [{:ok, _pid1}, {:ok, _pid2}] =
      Enum.map(1..2, fn(_) -> start_cloak_channel(@data_sources) end)
  end

  test "should unregister cloak when channel closes" do
    {:ok, pid} = start_cloak_channel(@data_sources)
    Process.unlink(pid)
    Process.exit(pid, :exit)

    assert soon([] == Cloak.channel_pids(@data_source_name))
  end

  test "should unregister cloak when channel closes, but retain alternative cloaks" do
    {:ok, pid1} = start_cloak_channel(@data_sources)
    {:ok, pid2} = start_cloak_channel(@data_sources)

    Process.unlink(pid1)
    Process.exit(pid1, :exit)

    assert soon(match?([{^pid2, _}], Cloak.channel_pids(@data_source_name)))
  end

  test "returns a list of cloaks and their data sources" do
    cloak_info = TestRepoHelper.cloak_info()
    Cloak.register(cloak_info, @data_sources)
    [cloak] = Cloak.all_cloak_infos()
    assert cloak.id == cloak_info.id
    assert cloak.name == cloak_info.name
    assert cloak.data_sources[@data_source_name] == @data_source
  end

  test "returns a list of cloaks for a data sources" do
    cloak_info = TestRepoHelper.cloak_info()
    Cloak.register(cloak_info, @data_sources)
    [cloak] = Cloak.cloak_infos_for_data_source(@data_source_name)
    assert cloak.id == cloak_info.id
    assert cloak.name == cloak_info.name
    assert cloak.data_sources[@data_source_name] == @data_source
  end

  describe "recording memory stats" do
    test "doesn't fail for unregistered cloak" do
      assert :ok == Cloak.record_memory(%{reading: true})
    end

    test "records memory for registered cloak" do
      Cloak.register(TestRepoHelper.cloak_info(), @data_sources)
      reading = %{reading: true}
      Cloak.record_memory(reading)
      assert [%{memory: ^reading}] = Cloak.all_cloak_infos()
    end

    test "has uninitialized memory reading by default" do
      Cloak.register(TestRepoHelper.cloak_info(), @data_sources)
      assert [%{memory: %{}}] = Cloak.all_cloak_infos()
    end
  end

  defp start_cloak_channel(data_sources) do
    parent = self()
    ref = make_ref()

    pid = spawn_link(fn ->
      registration_result = Cloak.register(TestRepoHelper.cloak_info(), data_sources)
      send(parent, {ref, registration_result})
      :timer.sleep(:infinity)
    end)

    receive do
      {^ref, registration_result} -> {registration_result, pid}
    end
  end

  # It seems that when the tests run too fast sometimes the (shared) GenServer under test manages to checkout a
  # connection that belongs to a test process that already died. I couldn't find an asynchronous operation that
  # would cause this, as the only operation on the GenServer is a call.
  defp wait_for_cleanup(), do: :timer.sleep(10)
end
