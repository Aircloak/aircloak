defmodule AirWeb.Admin.GroupController.Test do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  alias Air.{Schemas.Group, Repo}

  test "regular user can't manage users", %{conn: conn} do
    user = create_user!()
    group = create_group!()

    assert login(user) |> get(admin_group_path(conn, :index)) |> redirected_to() === "/"
    assert login(user) |> post(admin_group_path(conn, :create)) |> redirected_to() === "/"
    assert login(user) |> get(admin_group_path(conn, :edit, group)) |> redirected_to() === "/"

    assert login(user) |> delete(admin_group_path(conn, :delete, group)) |> redirected_to() === "/"
  end

  test "listing groups" do
    admin = create_admin_user!()
    groups = Enum.map(1..4, fn _ -> create_group!() end)
    groups_html = login(admin) |> get("/admin/groups") |> response(200)
    Enum.each(groups, &assert(groups_html =~ &1.name))
  end

  test "creating a group" do
    admin = create_admin_user!()
    group_name = "test group"

    assert login(admin)
           |> post("/admin/groups", group: %{name: group_name, admin: false})
           |> redirected_to() == admin_group_path(build_conn(), :index)

    groups_html = login(admin) |> get("/admin/groups") |> response(200)
    assert groups_html =~ group_name
  end

  test "displays a warning if no group name" do
    admin = create_admin_user!()

    assert login(admin)
           |> post("/admin/groups", group: %{admin: false})
           |> response(200) =~ ~r/something went wrong/
  end

  test "access edit page for group", %{conn: conn} do
    admin = create_admin_user!()
    group = create_group!()

    edit_html = login(admin) |> get(admin_group_path(conn, :edit, group)) |> response(200)
    assert edit_html =~ group.name
  end

  test "update a group", %{conn: conn} do
    admin = create_admin_user!()
    group = create_group!()
    new_group_name = "new group name"

    assert login(admin)
           |> put(
             admin_group_path(conn, :update, group),
             group: %{name: new_group_name, admin: !group.admin}
           )
           |> redirected_to() === admin_group_path(conn, :index)

    updated_group = Repo.get(Group, group.id)
    assert updated_group.name === new_group_name
    assert updated_group.admin != group.admin
  end

  test "error is reported when unsetting the admin status of the group containing last admin" do
    admin = create_only_admin_user!()
    conn = login(admin)
    conn = put(conn, admin_group_path(conn, :update, hd(admin.groups)), group: %{admin: false})

    assert redirected_to(conn) == "/admin/groups"

    assert get_flash(conn)["error"] ==
             "The given action cannot be performed, because it would remove the only administrator."

    assert Air.Service.User.active_admin_user_exists?()
  end

  test "deleting a group" do
    admin = create_admin_user!()
    group = create_group!()

    assert "/admin/groups" == login(admin) |> delete("/admin/groups/#{group.id}") |> redirected_to()

    groups_html = login(admin) |> get("/admin/groups") |> response(200)
    refute groups_html =~ group.name
  end

  test "error is reported when deleting the group containing last admin" do
    admin = create_only_admin_user!()
    conn = login(admin) |> delete("/admin/groups/#{hd(admin.groups).id}")

    assert redirected_to(conn) == "/admin/groups"

    assert get_flash(conn)["error"] ==
             "The given action cannot be performed, because it would remove the only administrator."

    assert Air.Service.User.active_admin_user_exists?()
  end

  test "render 404 on attempting to render edit form for non-existent group",
    do: assert(login(create_admin_user!()) |> get("/admin/groups/99999/edit") |> response(404))

  test "render 404 on attempting to update a non-existent group",
    do:
      assert(
        login(create_admin_user!())
        |> put("/admin/groups/99999", group: %{name: "group name"})
        |> response(404)
      )

  test "render 404 on attempting to delete a non-existent group",
    do: assert(login(create_admin_user!()) |> delete("/admin/groups/99999") |> response(404))
end
