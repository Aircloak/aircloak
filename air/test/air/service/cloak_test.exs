defmodule Air.Service.Cloak.Test do
  use ExUnit.Case, async: false
  use Air.SchemaCase

  import Aircloak.AssertionHelper

  alias Air.{Repo, TestRepoHelper, TestSocketHelper, Schemas.DataSource, Service.Cloak}

  @data_source_name "data_source_name"
  @data_source %{name: @data_source_name, tables: []}
  @data_sources [@data_source]
  @data_sources_that_differ [%{name: @data_source_name, tables: [%{different: true, columns: []}]}]

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
    Enum.each(1..10, fn _ -> start_cloak_channel(@data_sources) end)
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
    assert [_pid1, _pid2] = Enum.map(1..2, fn _ -> start_cloak_channel(@data_sources) end)
  end

  test "should unregister cloak when channel closes" do
    pid = start_cloak_channel(@data_sources)
    Process.unlink(pid)
    Process.exit(pid, :exit)

    assert soon([] == Cloak.channel_pids(@data_source_name))
  end

  test "should unregister cloak when channel closes, but retain alternative cloaks" do
    pid1 = start_cloak_channel(@data_sources)
    pid2 = start_cloak_channel(@data_sources)

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

  test "should record that a data source has conflicting definitions across cloaks" do
    Cloak.register(TestRepoHelper.cloak_info(), @data_sources)
    Cloak.register(TestRepoHelper.cloak_info("other_cloak"), @data_sources_that_differ)
    [error] = Poison.decode!(Repo.get_by!(DataSource, name: @data_source_name).errors)
    assert error =~ ~r/differs between .+ cloaks/
  end

  test "should not record differences in isolating stats when not all data sources done" do
    data_sources_done = [data_source_with_columns()]

    data_sources_pending = [
      data_source_with_columns([
        table_column(%{isolated: :pending})
      ])
    ]

    Cloak.register(TestRepoHelper.cloak_info(), data_sources_pending)
    Cloak.register(TestRepoHelper.cloak_info("other_cloak"), data_sources_done)
    assert [] == Poison.decode!(Repo.get_by!(DataSource, name: @data_source_name).errors)
  end

  test "should record differences in isolating stats when all data sources done" do
    data_sources_done1 = [
      data_source_with_columns([
        table_column(%{isolated: true})
      ])
    ]

    data_sources_done2 = [
      data_source_with_columns([
        table_column(%{isolated: false})
      ])
    ]

    Cloak.register(TestRepoHelper.cloak_info(), data_sources_done1)
    Cloak.register(TestRepoHelper.cloak_info("other_cloak"), data_sources_done2)
    refute [] == Poison.decode!(Repo.get_by!(DataSource, name: @data_source_name).errors)
  end

  test "should not record differences in shadow db stats when not all data sources done" do
    data_sources_done = [data_source_with_columns()]

    data_sources_pending = [
      data_source_with_columns([
        table_column(%{shadow_table: :pending})
      ])
    ]

    Cloak.register(TestRepoHelper.cloak_info(), data_sources_pending)
    Cloak.register(TestRepoHelper.cloak_info("other_cloak"), data_sources_done)
    assert [] == Poison.decode!(Repo.get_by!(DataSource, name: @data_source_name).errors)
  end

  test "should record differences in shadow db stats when all data sources done" do
    data_sources_done1 = [
      data_source_with_columns([
        table_column(%{shadow_table: :ok})
      ])
    ]

    data_sources_done2 = [
      data_source_with_columns([
        table_column(%{shadow_table: :unknown_column})
      ])
    ]

    Cloak.register(TestRepoHelper.cloak_info(), data_sources_done1)
    Cloak.register(TestRepoHelper.cloak_info("other_cloak"), data_sources_done2)
    refute [] == Poison.decode!(Repo.get_by!(DataSource, name: @data_source_name).errors)
  end

  test "should retain errors from all cloaks" do
    Cloak.register(TestRepoHelper.cloak_info("cloak1"), data_source_with_errors(["error 1"]))
    Cloak.register(TestRepoHelper.cloak_info("cloak2"), data_source_with_errors(["error 2"]))

    ["On cloak cloak2: error 2", "On cloak cloak1: error 1"] =
      Poison.decode!(Repo.get_by!(DataSource, name: @data_source_name).errors)
  end

  test "tags errors with the originating cloak, to help debug problems" do
    Cloak.register(TestRepoHelper.cloak_info("cloak1"), data_source_with_errors(["error"]))
    Cloak.register(TestRepoHelper.cloak_info("cloak2"), data_source_with_errors(["error"]))

    ["On cloak cloak2: error", "On cloak cloak1: error"] =
      Poison.decode!(Repo.get_by!(DataSource, name: @data_source_name).errors)
  end

  test "collecting running queries from connected cloaks" do
    {:connected, cloak1} = TestSocketHelper.connect(%{cloak_name: "cloak1"})
    TestSocketHelper.join!(cloak1, "main", %{data_sources: [%{name: "ds1", tables: []}]})

    {:connected, cloak2} = TestSocketHelper.connect(%{cloak_name: "cloak2"})
    TestSocketHelper.join!(cloak2, "main", %{data_sources: [%{name: "ds1", tables: []}]})

    task = Task.async(fn -> Cloak.running_queries() end)
    TestSocketHelper.respond_to_running_queries!(cloak1, ["foo", "bar"])
    TestSocketHelper.respond_to_running_queries!(cloak2, ["baz"])
    assert Enum.sort(Task.await(task)) == ["bar", "baz", "foo"]
  end

  defp data_source_with_columns(columns \\ [table_column()]) do
    %{
      name: @data_source_name,
      tables: [
        %{
          columns: columns,
          id: :accounts
        }
      ]
    }
  end

  defp table_column(params \\ %{}) do
    Map.merge(
      %{
        isolated: true,
        name: "column_name",
        shadow_table: :ok,
        shadow_table_size: 1,
        type: :integer,
        user_id: false
      },
      params
    )
  end

  defp data_source_with_errors(errors) do
    [Map.put(@data_source, :errors, errors)]
  end

  defp start_cloak_channel(data_sources) do
    parent = self()
    ref = make_ref()

    pid =
      spawn_link(fn ->
        Cloak.register(TestRepoHelper.cloak_info(), data_sources)
        send(parent, ref)
        :timer.sleep(:infinity)
      end)

    receive do
      ^ref -> pid
    end
  end

  # It seems that when the tests run too fast sometimes the (shared) GenServer under test manages to checkout a
  # connection that belongs to a test process that already died. I couldn't find an asynchronous operation that
  # would cause this, as the only operation on the GenServer is a call.
  defp wait_for_cleanup(), do: :timer.sleep(10)
end
