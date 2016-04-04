defmodule Air.UserControllerTest do
  use Air.ConnCase, async: false

  alias Air.{TestConnHelper, TestRepoHelper}

  test "regular user can't manage users" do
    org = TestRepoHelper.create_organisation!()
    user = TestRepoHelper.create_user!(org, :user)

    assert "/" == login(user) |> get("/users") |> redirected_to()
    assert "/" == login(user) |> get("/users/new") |> redirected_to()
    assert "/" == login(user) |> get("/users/#{user.id}/edit") |> redirected_to()
    assert "/" == login(user) |> post("/users") |> redirected_to()
    assert "/" == login(user) |> put("/users/#{user.id}") |> redirected_to()
    assert "/" == login(user) |> delete("/users/#{user.id}") |> redirected_to()
  end

  test "listing users" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)
    users = Enum.map(1..4, fn(_) -> TestRepoHelper.create_user!(org, :user) end)

    users_html = login(org_admin) |> get("/users") |> response(200)
    Enum.each([org_admin | users], &assert(users_html =~ &1.email))
  end

  test "accessing new and edit forms" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    login(org_admin) |> get("/users/new") |> response(200)
    login(org_admin) |> get("/users/#{org_admin.id}/edit") |> response(200)
  end

  test "creating a user" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    new_user_email = "foo@bar.baz"

    conn =
      login(org_admin)
      |> post("/users", user: %{
            email: new_user_email,
            name: "foobarbaz",
            password: "1234",
            password_confirmation: "1234",
            role_id: Air.User.role_id(:user),
            organisation_id: org.id
          })

    assert "/users" == redirected_to(conn)
    users_html = login(org_admin) |> get("/users") |> response(200)
    assert users_html =~ new_user_email
  end

  test "updating a user" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)

    changed_email = "foo@bar.baz"

    conn =
      login(org_admin)
      |> put("/users/#{org_admin.id}", user: %{
            email: changed_email,
            name: org_admin.name,
            role_id: org_admin.role_id,
            organisation_id: org.id
          })

    assert "/users" == redirected_to(conn)
    users_html = login(org_admin) |> get("/users") |> response(200)
    assert users_html =~ changed_email
    refute users_html =~ org_admin.email
  end

  test "deleting a user" do
    org = TestRepoHelper.create_organisation!()
    org_admin = TestRepoHelper.create_user!(org, :org_admin)
    user = TestRepoHelper.create_user!(org, :user)

    assert "/users" == login(org_admin) |> delete("/users/#{user.id}") |> redirected_to()
    users_html = login(org_admin) |> get("/users") |> response(200)
    refute users_html =~ user.email
  end

  defp login(user),
    do: TestConnHelper.login(conn(), user)
end
