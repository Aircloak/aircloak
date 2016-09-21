defmodule Air.Admin.UserControllerTest do
  use Air.ConnCase, async: true

  import Air.TestConnHelper
  alias Air.TestRepoHelper

  test "regular user can't manage users" do
    user = TestRepoHelper.create_user!()

    assert "/" == login(user) |> get("/admin/users") |> redirected_to()
    assert "/" == login(user) |> get("/admin/users/new") |> redirected_to()
    assert "/" == login(user) |> get("/admin/users/#{user.id}/edit") |> redirected_to()
    assert "/" == login(user) |> post("/admin/users") |> redirected_to()
    assert "/" == login(user) |> put("/admin/users/#{user.id}") |> redirected_to()
    assert "/" == login(user) |> delete("/admin/users/#{user.id}") |> redirected_to()
  end

  test "listing users" do
    admin = TestRepoHelper.create_admin_user!()
    users = Enum.map(1..4, fn(_) -> TestRepoHelper.create_user!() end)

    users_html = login(admin) |> get("/admin/users") |> response(200)
    Enum.each([admin | users], &assert(users_html =~ &1.email))
  end

  test "accessing new and edit forms" do
    admin = TestRepoHelper.create_admin_user!()

    login(admin) |> get("/admin/users/new") |> response(200)
    login(admin) |> get("/admin/users/#{admin.id}/edit") |> response(200)
  end

  test "creating a user" do
    admin = TestRepoHelper.create_admin_user!()

    new_user_email = "foo@bar.baz"

    conn = login(admin)
    |> post("/admin/users", user: %{
      email: new_user_email,
      name: "foobarbaz",
      password: "1234",
      password_confirmation: "1234",
    })

    assert "/admin/users" == redirected_to(conn)
    users_html = login(admin) |> get("/admin/users") |> response(200)
    assert users_html =~ new_user_email
  end

  test "updating a user" do
    admin = TestRepoHelper.create_admin_user!()

    changed_email = "foo@bar.baz"

    conn = login(admin)
    |> put("/admin/users/#{admin.id}", user: %{
      email: changed_email,
      name: admin.name
    })

    assert "/admin/users" == redirected_to(conn)
    users_html = login(%{email: changed_email}) |> get("/admin/users") |> response(200)
    assert users_html =~ changed_email
    refute users_html =~ admin.email
  end

  test "deleting a user" do
    admin = TestRepoHelper.create_admin_user!()
    user = TestRepoHelper.create_user!()

    assert "/admin/users" == login(admin) |> delete("/admin/users/#{user.id}") |> redirected_to()
    users_html = login(admin) |> get("/admin/users") |> response(200)
    refute users_html =~ user.email
  end
end
