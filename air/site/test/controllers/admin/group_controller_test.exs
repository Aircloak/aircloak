defmodule Air.Admin.GroupControllerTest do
  use Air.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper

  test "regular user can't manage users" do
    user = TestRepoHelper.create_user!()
    group = TestRepoHelper.create_group!()

    assert login(user) |> get("/admin/groups") |> redirected_to() === "/"
    assert login(user) |> post("/admin/groups") |> redirected_to() === "/"
    assert login(user) |> delete("/admin/groups/#{group.id}") |> redirected_to() === "/"
  end

  test "listing groups" do
    admin = TestRepoHelper.create_admin_user!()
    groups = Enum.map(1..4, fn(_) -> TestRepoHelper.create_group!() end)
    groups_html = login(admin) |> get("/admin/groups") |> response(200)
    Enum.each(groups, &assert(groups_html =~ &1.name))
  end

  test "creating a group" do
    admin = TestRepoHelper.create_admin_user!()

    group_name = "test group"

    conn = login(admin)
    |> post("/admin/groups", group: %{name: group_name, admin: false})

    assert "/admin/groups" == redirected_to(conn)
    groups_html = login(admin) |> get("/admin/groups") |> response(200)
    assert groups_html =~ group_name
  end

  test "deleting a group" do
    admin = TestRepoHelper.create_admin_user!()
    group = TestRepoHelper.create_group!()

    assert "/admin/groups" == login(admin) |> delete("/admin/groups/#{group.id}") |> redirected_to()
    groups_html = login(admin) |> get("/admin/groups") |> response(200)
    refute groups_html =~ group.name
  end
end
