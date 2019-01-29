defmodule AirWeb.Admin.UserController.Test do
  use AirWeb.ConnCase, async: false

  import Air.{TestConnHelper, TestRepoHelper}
  import Aircloak.AssertionHelper

  alias Air.Service.User

  test "regular user can't manage users" do
    user = create_user!()

    assert "/" == login(user) |> get("/admin/users") |> redirected_to()
    assert "/" == login(user) |> get("/admin/users/new") |> redirected_to()
    assert "/" == login(user) |> get("/admin/users/#{user.id}/edit") |> redirected_to()
    assert "/" == login(user) |> post("/admin/users") |> redirected_to()
    assert "/" == login(user) |> put("/admin/users/#{user.id}") |> redirected_to()
    assert "/" == login(user) |> delete("/admin/users/#{user.id}") |> redirected_to()
  end

  test "listing users" do
    admin = create_admin_user!()
    users = Enum.map(1..4, fn _ -> create_user!() end)

    users_html = login(admin) |> get("/admin/users") |> response(200)
    Enum.each([admin | users], &assert(users_html =~ User.main_login(&1)))
  end

  test "accessing new and edit forms" do
    admin = create_admin_user!()

    login(admin) |> get("/admin/users/new") |> response(200)
    login(admin) |> get("/admin/users/#{admin.id}/edit") |> response(200)
  end

  test "creating a user" do
    admin = create_admin_user!()

    new_user_login = "foo@bar.baz"

    conn =
      login(admin)
      |> post(
        "/admin/users",
        user: %{
          login: new_user_login,
          name: "foobarbaz",
          password: "password1234",
          password_confirmation: "password1234"
        }
      )

    assert redirected_to(conn) =~ ~r[admin/users/.*/edit]
    users_html = login(admin) |> get("/admin/users") |> response(200)
    assert users_html =~ new_user_login
  end

  test "updating a user" do
    admin = create_admin_user!()

    changed_login = "foo@bar.baz"

    conn =
      login(admin)
      |> put(
        "/admin/users/#{admin.id}",
        user: %{
          login: changed_login,
          name: admin.name
        }
      )

    assert "/admin/users" == redirected_to(conn)
    users_html = login(%{logins: [%{login: changed_login}]}) |> get("/admin/users") |> response(200)
    assert users_html =~ changed_login
    refute users_html =~ User.main_login(admin)
  end

  test "error is reported when updating tha last admin to non-admin status" do
    admin = create_only_user_as_admin!()

    conn = login(admin) |> put("/admin/users/#{admin.id}", user: %{groups: []})

    assert "/admin/users" == redirected_to(conn)

    assert get_flash(conn)["error"] ==
             "The given action cannot be performed, because it would remove the only administrator."

    assert User.active_admin_user_exists?()
  end

  test "deleting a user" do
    admin = create_admin_user!()
    user = create_user!()

    assert "/admin/users" == login(admin) |> delete("/admin/users/#{user.id}") |> redirected_to()
    users_html = login(admin) |> get("/admin/users") |> response(200)
    refute users_html =~ User.main_login(user)
  end

  test "disabling a user" do
    admin = create_admin_user!()
    user = create_user!()

    assert "/admin/users" == login(admin) |> put("/admin/users/#{user.id}/disable") |> redirected_to()

    refute User.load(user.id).enabled
  end

  test "enabling a user" do
    admin = create_admin_user!()
    user = create_user!()

    {:ok, disabled_user} = User.disable(user)
    refute disabled_user.enabled

    assert "/admin/users" == login(admin) |> put("/admin/users/#{user.id}/enable") |> redirected_to()

    assert User.load(user.id).enabled
  end

  test "success is reported via audit log" do
    admin = create_admin_user!()
    user = create_user!()

    login(admin) |> delete("/admin/users/#{user.id}")

    assert soon(Air.Repo.get_by(Air.Schemas.AuditLog, user_id: admin.id, event: "User removal succeeded"))
  end

  test "render 404 on attempting to render edit form for non-existent user" do
    admin = create_admin_user!()
    assert login(admin) |> get("/admin/users/99999/edit") |> response(404)
  end

  test "render 404 on attempting to update a non-existent user" do
    admin = create_admin_user!()

    assert login(admin)
           |> put("/admin/users/99999", user: %{login: "some@email.com", name: "some name"})
           |> response(404)
  end

  test "render 404 on attempting to delete a non-existent user" do
    admin = create_admin_user!()
    assert login(admin) |> delete("/admin/users/99999") |> response(404)
  end

  describe ".delete_sessions" do
    setup do
      {:ok, admin: create_admin_user!(), user: create_user!()}
    end

    test "redirects back to user edit", %{admin: admin, user: user} do
      assert "/admin/users/#{user.id}/edit" ==
               admin |> login() |> delete("/admin/users/#{user.id}/sessions") |> redirected_to()
    end

    test "invalidates the user's sessions", %{admin: admin, user: user} do
      session = Air.Service.RevokableToken.sign(:data, user, :session)

      admin |> login() |> delete("/admin/users/#{user.id}/sessions")

      assert {:error, :invalid_token} = Air.Service.RevokableToken.verify(session, :session, max_age: :infinity)
    end
  end
end
