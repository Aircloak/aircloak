defmodule Air.Admin.CloaksControllerTest do
  # `async: false` because shared sandbox mode is used
  # (see https://hexdocs.pm/ecto/Ecto.Adapters.SQL.Sandbox.html)
  use Air.ConnCase, async: false

  import Air.{TestConnHelper, AssertionHelper, TestUtils}
  alias Air.{TestRepoHelper, DataSourceManager}

  setup do
    Ecto.Adapters.SQL.Sandbox.mode(Repo, {:shared, self()})
  end

  test "anonymous user can't access the page", %{conn: conn} do
    conn |> get("/admin/cloaks") |> redirected_to()
  end

  test "only shows cloaks while they are online" do
    admin_org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(admin_org, :org_admin)

    # connect a mock cloak
    {terminator, pid} = temporary_process()
    cloak_info = %{
      channel_pid: pid,
      id: "cloak id",
      name: "cloak name",
      online_since: Timex.DateTime.now(),
    }

    data_sources = [%{"global_id" => "global_id", "tables" => []}]
    DataSourceManager.register_cloak(cloak_info, data_sources)

    # verify that it's in the list
    html_response = login(admin) |> get("/admin/cloaks") |> response(200)
    assert html_response =~ "cloak name"

    # disconnect the cloak
    terminator.()

    # verify that it's in the list
    refute soon((login(admin) |> get("/admin/cloaks") |> response(200)) =~ "test_cloak1")
  end

  test "only shows cloaks to admin" do
    org = TestRepoHelper.create_organisation!("unknown_org")
    user = TestRepoHelper.create_user!(org, :user)
    admin_org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(admin_org, :org_admin)

    # connect a mock cloak
    {terminator, pid} = temporary_process()
    cloak_info = %{
      channel_pid: pid,
      id: "cloak id",
      name: "cloak name",
      online_since: Timex.DateTime.now(),
    }

    data_sources = [%{"global_id" => "global_id", "tables" => []}]
    DataSourceManager.register_cloak(cloak_info, data_sources)

    # verify that it's in the list
    login(user) |> get("/admin/cloaks") |> redirected_to()
    html_response = login(admin) |> get("/admin/cloaks") |> response(200)
    assert html_response =~ "cloak name"

    # disconnect the cloak
    terminator.()

    # verify that it's in the list
    refute soon((login(admin) |> get("/admin/cloaks") |> response(200)) =~ "test_cloak1")
  end
end
