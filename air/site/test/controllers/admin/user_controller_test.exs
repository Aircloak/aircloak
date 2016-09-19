defmodule Air.Admin.UserControllerTest do
  use Air.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper

  test "regular user can't manage users" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :user)

    assert "/" == login(user) |> get("/admin/users") |> redirected_to()
    assert "/" == login(user) |> get("/admin/users/new") |> redirected_to()
    assert "/" == login(user) |> get("/admin/users/#{user.id}/edit") |> redirected_to()
    assert "/" == login(user) |> post("/admin/users") |> redirected_to()
    assert "/" == login(user) |> put("/admin/users/#{user.id}") |> redirected_to()
    assert "/" == login(user) |> delete("/admin/users/#{user.id}") |> redirected_to()
  end

  test "listing users" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)
    users = Enum.map(1..4, fn(_) -> TestRepoHelper.create_user!(org, :user) end)

    users_html = login(org_admin) |> get("/admin/users") |> response(200)
    Enum.each([org_admin | users], &assert(users_html =~ &1.email))
  end

  test "accessing new and edit forms" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    login(org_admin) |> get("/admin/users/new") |> response(200)
    login(org_admin) |> get("/admin/users/#{org_admin.id}/edit") |> response(200)
  end

  test "admins can change user's organisation" do
    org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(org) |> TestRepoHelper.make_admin!()

    html = login(admin) |> get("/admin/users/new") |> response(200)
    assert html =~ ~s(name="user[organisation_id]")

    html = login(admin) |> get("/admin/users/#{admin.id}/edit") |> response(200)
    assert html =~ ~s(name="user[organisation_id]")
  end

  test "org admins can't change user's organisation" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    html = login(org_admin) |> get("/admin/users/new") |> response(200)
    refute html =~ ~s(name="user[organisation_id]")

    html = login(org_admin) |> get("/admin/users/#{org_admin.id}/edit") |> response(200)
    refute html =~ ~s(name="user[organisation_id]")
  end

  test "creating a user" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    new_user_email = "foo@bar.baz"

    conn = login(org_admin)
    |> post("/admin/users", user: %{
      email: new_user_email,
      name: "foobarbaz",
      password: "1234",
      password_confirmation: "1234",
      role_id: Air.User.role_id(:user),
      organisation_id: org.id
    })

    assert "/admin/users" == redirected_to(conn)
    users_html = login(org_admin) |> get("/admin/users") |> response(200)
    assert users_html =~ new_user_email
  end

  test "admin can create a user in another org" do
    admin = TestRepoHelper.create_admin_user!()
    another_org = TestRepoHelper.create_organisation!()

    new_user_email = "foo@bar.baz"

    conn = login(admin)
    |> post("/admin/users", user: %{
      email: new_user_email,
      name: "foobarbaz",
      password: "1234",
      password_confirmation: "1234",
      role_id: Air.User.role_id(:user),
      organisation_id: another_org.id
    })

    assert "/admin/users" == redirected_to(conn)
    users_html = login(admin) |> get("/admin/users") |> response(200)
    assert users_html =~ new_user_email
  end

  test "updating a user" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    changed_email = "foo@bar.baz"

    conn = login(org_admin)
    |> put("/admin/users/#{org_admin.id}", user: %{
      email: changed_email,
      name: org_admin.name,
      role_id: org_admin.role_id
    })

    assert "/admin/users" == redirected_to(conn)
    users_html = login(%{email: changed_email}) |> get("/admin/users") |> response(200)
    assert users_html =~ changed_email
    refute users_html =~ org_admin.email
  end

  test "admin can change user's org" do
    admin_org = TestRepoHelper.admin_organisation()
    admin = TestRepoHelper.create_user!(admin_org) |> TestRepoHelper.make_admin!()
    another_org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(another_org, :user)

    conn = login(admin)
    |> put("/admin/users/#{user.id}", user: %{organisation_id: admin_org.id})

    assert "/admin/users" == redirected_to(conn)

    users_html = login(admin) |> get("/admin/organisations/#{admin_org.id}") |> response(200)
    assert users_html =~ user.email
  end

  test "org admin can't change user's org" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    changed_email = "foo@bar.baz"

    conn = login(org_admin)
    |> put("/admin/users/#{org_admin.id}", user: %{
      email: changed_email,
      name: org_admin.name,
      role_id: org_admin.role_id,
      organisation_id: TestRepoHelper.admin_organisation().id
    })

    assert "/admin/users" == redirected_to(conn)
    assert get_flash(conn)["error"] =~ "Action not allowed!"

    users_html = login(org_admin) |> get("/admin/users") |> response(200)
    refute users_html =~ changed_email
    assert users_html =~ org_admin.email
  end

  test "deleting a user" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)
    user = TestRepoHelper.create_user!(org, :user)

    assert "/admin/users" == login(org_admin) |> delete("/admin/users/#{user.id}") |> redirected_to()
    users_html = login(org_admin) |> get("/admin/users") |> response(200)
    refute users_html =~ user.email
  end
end
