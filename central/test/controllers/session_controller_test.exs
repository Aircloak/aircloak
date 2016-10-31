defmodule Central.SessionControllerTest do
  use Central.ConnCase, async: true

  import Central.TestConnHelper
  alias Central.TestRepoHelper

  test "anonymous user can access the login page", %{conn: conn} do
    conn |> get("/auth") |> response(200)
  end

  test "logging in/out" do
    user = TestRepoHelper.create_user!()

    # invalid e-mail
    html = build_conn() |> post("/auth", email: "foo@aircloak.com", password: "1234") |> response(200)
    assert html =~ "Invalid e-mail or password"

    # invalid password
    html = build_conn() |> post("/auth", email: user.email, password: "") |> response(200)
    assert html =~ "Invalid e-mail or password"

    # correct login
    logged_in_conn = build_conn() |> post("/auth", email: user.email, password: "1234")
    assert "/" == redirected_to(logged_in_conn)
    assert get_flash(logged_in_conn)["info"] =~ "Logged in successfully"
    # verify that the user can now access a page requiring authentication
    recycle(logged_in_conn) |> get("/users") |> response(200)

    # verify that we can now logout
    logged_out_conn = recycle(logged_in_conn) |> delete("/logout")
    assert "/auth" == redirected_to(logged_out_conn)
    assert get_flash(logged_out_conn)["info"] =~ "Logged out successfully"
  end

  test "logged in user can't log in" do
    user = TestRepoHelper.create_user!()

    response = login(user) |> get("/auth") |> response(400)
    assert response =~ "already authenticated"

    response = login(user) |> post("/auth", email: "", password: "") |> response(400)
    assert response =~ "already authenticated"
  end

  test "anonymous user can't log out", %{conn: conn} do
    conn = conn |> delete("/logout")
    assert "/auth" == redirected_to(conn)
    assert get_flash(conn)["error"] =~ "You must be authenticated to view this page"
  end
end
