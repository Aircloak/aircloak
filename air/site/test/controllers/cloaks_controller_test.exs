defmodule Air.CloaksControllerTest do
  # Despite using Ecto 2.0 with it's transactional DB sandbox model,
  # we have to run these tests sequentially.
  # The problem causing the sequential execution is that the database pool
  # is used from a process distinct from the test one:
  #
  # Ecto provides two options:
  #
  # - explicitly allowing a third process to access a sandbox pool
  # - sharing the test pool with the world
  #
  # Explicitly allowing the DataSourceManager access doesn't work as
  # we would have to concurrently give it access to multiple test pools,
  # which then in turns means it wouldn't know which to check out a connection from.
  #
  # Using distinct servers per test doesn't work either, since we don't
  # control the calling site.
  #
  # The sharing option is the one we are using, but since any process can access
  # the pool, we cannot run tests concurrently.
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, AssertionHelper, TestUtils}
  alias Air.{TestRepoHelper, DataSourceManager}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  test "anonymous user can't access the page", %{conn: conn} do
    conn |> get("/cloaks") |> redirected_to()
  end

  test "only shows cloaks while they are online" do
    org = TestRepoHelper.create_organisation!("unknown_org")
    user = TestRepoHelper.create_user!(org, :user)

    # connect a mock cloak
    {terminator, pid} = temporary_process()
    cloak_info = %{
      channel_pid: pid,
      id: "cloak id",
      name: "cloak name",
      online_since: Timex.DateTime.now(),
    }

    data_sources = [%{"id" => "id", "name" => "name", "tables" => []}]
    DataSourceManager.register_cloak(cloak_info, data_sources)

    # verify that it's in the list
    html_response = login(user) |> get("/cloaks") |> response(200)
    assert html_response =~ "cloak name"

    # disconnect the cloak
    terminator.()

    # verify that it's in the list
    refute soon((login(user) |> get("/cloaks") |> response(200)) =~ "test_cloak1")
  end
end
