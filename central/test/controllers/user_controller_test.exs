defmodule Central.UserControllerTest do
  use CentralWeb.ConnCase, async: true

  import Central.TestConnHelper
  alias Central.TestRepoHelper

  test "listing users" do
    user = TestRepoHelper.create_user!()
    users = Enum.map(1..4, fn _ -> TestRepoHelper.create_user!() end)

    users_html = login(user) |> get("/users") |> response(200)
    Enum.each([user | users], &assert(users_html =~ &1.email))
  end

  test "accessing new and edit forms" do
    user = TestRepoHelper.create_user!()

    login(user) |> get("/users/new") |> response(200)
    login(user) |> get("/users/#{user.id}/edit") |> response(200)
  end

  test "creating a user" do
    user = TestRepoHelper.create_user!()

    new_user_email = "foo@bar.baz"

    conn =
      login(user)
      |> post(
        "/users",
        user: %{
          email: new_user_email,
          name: "foobarbaz",
          password: "1234",
          password_confirmation: "1234"
        }
      )

    assert "/users" == redirected_to(conn)
    users_html = login(user) |> get("/users") |> response(200)
    assert users_html =~ new_user_email
  end

  test "updating a user" do
    user = TestRepoHelper.create_user!()

    changed_email = "foo@bar.baz"

    conn =
      login(user)
      |> put(
        "/users/#{user.id}",
        user: %{
          email: changed_email,
          name: user.name
        }
      )

    assert "/users" == redirected_to(conn)
    users_html = login(%{email: changed_email}) |> get("/users") |> response(200)
    assert users_html =~ changed_email
    refute users_html =~ user.email
  end

  test "deleting a user" do
    user = TestRepoHelper.create_user!()
    deletable_user = TestRepoHelper.create_user!()

    assert "/users" == login(user) |> delete("/users/#{deletable_user.id}") |> redirected_to()
    users_html = login(user) |> get("/users") |> response(200)
    refute users_html =~ deletable_user.email
  end

  test "render 404 on attempting to render edit form for non-existent user" do
    user = TestRepoHelper.create_user!()
    assert login(user) |> get("/users/99999/edit") |> response(404)
  end

  test "render 404 on attempting to update a non-existent user" do
    user = TestRepoHelper.create_user!()

    assert login(user)
           |> put("/users/99999", user: %{email: "some@email.com", name: "some name"})
           |> response(404)
  end

  test "render 404 on attempting to delete a non-existent user" do
    user = TestRepoHelper.create_user!()
    assert login(user) |> delete("/users/99999") |> response(404)
  end
end
